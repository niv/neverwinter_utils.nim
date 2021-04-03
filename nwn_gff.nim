import shared

const SupportedFormatsSimple = ["gff", "json"]
const SupportedFormats = {
  "json": @["json"],
  "gff": GffExtensions
}.toTable

let args = DOC """
Convert gff data to json.

The json data is compatible with https://github.com/niv/nwn-lib.

Supported input/output formats: """ & SupportedFormatsSimple.join(", ") & """


Input and output default to stdin/stdout respectively.

Usage:
  $0 [options]
  $USAGE

Options:
  -i IN                       Input file [default: -]
  -l INFORMAT                 Input format [default: autodetect]
  --in-sqlite FILE            Squash the given SQLite database into the struct
                              after reading IN. Only some GFF formats support
                              embedded SQLite databases. This will clobber any
                              SQLite data already present in IN.

  -o OUT                      Output file [default: -]
  -k OUTFORMAT                Output format [default: autodetect]
  --out-sqlite FILE           Extract the SQLite contained in the operated-on
                              file. Only some GFF formats support embedded SQLite
                              databases.

  -p, --pretty                Pretty output (json only)
  -u, --unsorted              Don't sort keys (json only)
  $OPT
"""

let inputfile   = $args["-i"]
let outputfile  = $args["-o"]
let informat    = ensureValidFormat($args["-l"], inputfile, SupportedFormats)
let outformat   = ensureValidFormat($args["-k"], outputfile, SupportedFormats)

let input  = if $args["-i"] == "-": newFileStream(stdin) else: openFileStream($args["-i"])
let output = if $args["-o"] == "-": newFileStream(stdout) else: openFileStream($args["-o"], fmWrite)

var state: GffRoot

case informat:
of "gff":    state = input.readGffRoot(false)
of "json":   state = input.parseJson(inputfile).gffRootFromJson()
else: quit("Unsupported informat: " & informat)

proc postProcessJson(j: JsonNode) =
  ## Post-process json before emitting: We make sure to re-sort.
  if j.kind == JObject:
    for k, v in j.fields: postProcessJson(v)
    j.fields.sort do (a, b: auto) -> int: cmpIgnoreCase(a[0], b[0])
  elif j.kind == JArray:
    for e in j.elems: postProcessJson(e)

if args["--in-sqlite"]:
  let blob = compress(readFile($args["--in-sqlite"]), Algorithm.Zstd, makeMagic("SQL3"))
  state["SQLite", GffStruct] = newGffStruct(10)
  state["SQLite", GffStruct]["Data", GffVoid] = blob.GffVoid
  state["SQLite", GffStruct]["Size", GffDword] = blob.len.GffDword

if args["--out-sqlite"]:
  if state.hasField("SQLite", GffStruct) and state["SQLite", GffStruct].hasField("Data", GffVoid):
    let blob = state["SQLite", GffStruct]["Data", GffVoid].string
    writeFile($args["--out-sqlite"], decompress(blob, makeMagic("SQL3")))

case outformat:
of "gff":    output.write(state)
of "json":
             let j = state.toJson()
             if not args["--unsorted"]:
               postProcessJson(j)
             output.write(if args["--pretty"]: j.pretty() else: $j)
             output.write("\c\L")
else: quit("Unsupported outformat: " & outformat)
