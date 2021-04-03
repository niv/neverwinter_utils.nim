import strutils, sequtils, algorithm, streams, sugar, tables, sets, encodings,
  typetraits

import util, languages
export languages

type
  GffStruct* = ref object of RootObj
    root: GffRoot
    id*: int32
    fields: OrderedTableRef[string, GffField]

  GffByte* = uint8
  GffChar* = int8
  GffWord* = uint16
  GffShort* = int16
  GffDword* = uint32
  GffInt* = int32
  GffFloat* = float32

  GffDword64* = uint64
  GffInt64* = int64
  GffDouble* = float64
  GffCExoString* = string
  GffResRef* = distinct string
  GffCExoLocString* = ref object
    strRef*: StrRef
    entries*: TableRef[int, string]

  GffVoid* = distinct string

  GffFieldKind* {.pure.} = enum
    Byte = 0
    Char = 1
    Word = 2
    Short = 3
    Dword = 4
    Int = 5
    Dword64 = 6
    Int64 = 7
    Float = 8
    Double = 9
    CExoString = 10
    ResRef = 11
    CExoLocString = 12
    Void = 13
    Struct = 14
    List = 15

  GffList* = seq[GffStruct]

  GffFieldTypeSimple* =
    GffByte |
    GffChar |
    GffWord |
    GffShort |
    GffDword |
    GffInt |
    GffFloat

  GffFieldTypeComplex* =
    GffDword64 |
    GffInt64 |
    GffDouble |
    GffCExoString |
    GffResRef |
    GffCExoLocString |
    GffVoid |
    GffStruct |
    GffList

  GffFieldType* = GffFieldTypeSimple | GffFieldTypeComplex

  GffRoot* = ref object of GffStruct
    fileType*: string
    fileVersion*: string
    loader: GffLazyLoader

  GffField* = ref object
    struct: GffStruct # used by the lazyloader
    resolved: bool # true if this field was already resolved by the loader

    dataOrOffset: int

    case fieldKind: GffFieldKind #nb: simple types are stored in dataOrOffset
    of GffFieldKind.Dword64: gffDword64: GffDword64
    of GffFieldKind.Int64: gffInt64: GffInt64
    of GffFieldKind.Double: gffDouble: GffDouble
    of GffFieldKind.CExoString: gffCExoString: GffCExoString
    of GffFieldKind.ResRef: gffResRef: GffResRef
    of GffFieldKind.CExoLocString: gffCExoLocString: GffCExoLocString
    of GffFieldKind.Void: gffVoid: GffVoid
    of GffFieldKind.Struct: gffStruct: GffStruct
    of GffFieldKind.List: gffList: GffList
    else: discard

  GffRootStructEntry = tuple
    id: int32
    dataOrOffset: int
    fieldCount: int
    resolved: bool # structs can only be loaded once

  GffRootFieldEntry = tuple
    fieldKind: GffFieldKind
    labelIndex: int
    dataOrOffset: int
    resolved: bool

  # # temp data needed for lazyloading
  GffLazyLoader = ref object
    root: GffRoot
    ## Indicates that data from this gff root is loaded lazily.
    ## This implies that the io handle that data is read from needs
    ## to be kept open.
    lazy: bool

    io: Stream
    ioStartPosition: int

    ## Unpacked data. Read-only view from `io`.
    labelsArray: seq[string]
    structs: seq[GffRootStructEntry]
    fieldIndices: seq[int]
    fieldsArray: seq[GffRootFieldEntry]
    listIndices: seq[int]

    header: tuple[
      structOffset:       int32,
      structCount:        int32,
      fieldOffset:        int32,
      fieldCount:         int32,
      labelOffset:        int32,
      labelCount:         int32,
      fieldDataOffset:    int32,
      fieldDataSize:      int32,
      fieldIndicesOffset: int32,
      fieldIndicesSize:   int32,
      listIndicesOffset:  int32,
      listIndicesSize:    int32
    ]

proc `$`*(s: GffCExoLocString): string =
  "[$1]$2" % [$s.strref, $s.entries]

proc isComplexType*(k: GffFieldKind): bool =
  ## Returns true if the given `k` is a complex type (has data at an offset
  ## instead of inline.)
  result = k in [ GffFieldKind.Dword64,
    GffFieldKind.Int64, GffFieldKind.Double, GffFieldKind.CExoString,
    GffFieldKind.ResRef, GffFieldKind.CExoLocString, GffFieldKind.Void,
    GffFieldKind.Struct, GffFieldKind.List ]

proc typeDescToKind*[T : GffFieldType](ofType: typedesc[T]): GffFieldKind {.inline.} =
  ## Template to convert a typedesc into a GffFieldKind.
  when ofType is GffByte: result = GffFieldKind.Byte
  elif ofType is GffChar: result = GffFieldKind.Char
  elif ofType is GffWord: result = GffFieldKind.Word
  elif ofType is GffShort: result = GffFieldKind.Short
  elif ofType is GffDword: result = GffFieldKind.Dword
  elif ofType is GffInt: result = GffFieldKind.Int
  elif ofType is GffDword64: result = GffFieldKind.Dword64
  elif ofType is GffInt64: result = GffFieldKind.Int64
  elif ofType is GffFloat: result = GffFieldKind.Float
  elif ofType is GffDouble: result = GffFieldKind.Double
  elif ofType is GffCExoString: result = GffFieldKind.CExoString
  elif ofType is GffResRef: result = GffFieldKind.ResRef
  elif ofType is GffVoid: result = GffFieldKind.Void
  elif ofType is GffCExoLocString: result = GffFieldKind.CExoLocString
  elif ofType is GffList: result = GffFieldKind.List
  elif ofType is GffStruct: result = GffFieldKind.Struct
  else: raise newException(ValueError, "type not a valid gff kind: " & name(ofType))

proc hasTypeOf*[T: GffFieldType](fieldKind: GffFieldKind, ofType: typedesc[T]): bool =
  ## Template to check if if fieldKind is representable by the given typedesc.
  result = false

  case fieldKind:
    of GffFieldKind.Byte:
      when ofType is byte: result = true
    of GffFieldKind.Char:
      when ofType is GffChar: result = true
    of GffFieldKind.Word:
      when ofType is GffWord: result = true
    of GffFieldKind.Short:
      when ofType is GffShort: result = true
    of GffFieldKind.Dword:
      when ofType is GffDword: result = true
    of GffFieldKind.Int:
      when ofType is GffInt: result = true
    of GffFieldKind.Dword64:
      when ofType is GffDword64: result = true
    of GffFieldKind.Int64:
      when ofType is GffInt64: result = true
    of GffFieldKind.Float:
      when ofType is GffFloat: result = true
    of GffFieldKind.Double:
      when ofType is GffDouble: result = true
    of GffFieldKind.CExoString:
      when ofType is GffCExoString: result = true
    of GffFieldKind.ResRef:
      when ofType is GffResRef: result = true
    of GffFieldKind.CExoLocString:
      when ofType is GffCExoLocString: result = true
    of GffFieldKind.Void:
      when ofType is GffVoid: result = true
    of GffFieldKind.Struct:
      when ofType is GffStruct: result = true
    of GffFieldKind.List:
      when ofType is GffList: result = true

proc ensureTypeOf*[T: GffFieldType](fieldKind: GffFieldKind, ofType: typedesc[T]) =
  ## Same as `hasTypeOf`, except this will raise a ValueError on mismatch.
  if not hasTypeOf(fieldKind, ofType):
    raise newException(ValueError, "FieldKind `" & $fieldKind &
      "` not castable to `" & name(ofType) & "`")

proc fieldKind*(self: GffField): GffFieldKind = self.fieldKind

proc resolve(self: GffField): GffField

proc getValue*[T: GffFieldType](self: GffField, t: typedesc[T]): T =
  discard self.resolve()

  when T is GffByte: cast[uint8](self.dataOrOffset)
  elif T is GffChar: cast[int8](self.dataOrOffset)
  elif T is GffWord: cast[uint16](self.dataOrOffset)
  elif T is GffShort: cast[int16](self.dataOrOffset)
  elif T is GffDword: cast[uint32](self.dataOrOffset)
  elif T is GffInt: cast[int32](self.dataOrOffset)
  elif T is GffFloat: cast[float32](self.dataOrOffset)

  elif T is GffDword64: self.gffDword64
  elif T is GffInt64: self.gffInt64
  elif T is GffDouble: self.gffDouble
  elif T is GffCExoString: self.gffCExoString
  elif T is GffResRef: self.gffResRef
  elif T is GffCExoLocString: self.gffCExoLocString
  elif T is GffList: self.gffList
  elif T is GffStruct: self.gffStruct
  elif T is GffVoid: self.gffVoid
  else: {.fatal: "You added a gff type but didn't make it resolvable in getValue()"}

proc assignValue*[T: GffFieldType](self: GffField, v: T) =
  when T is GffByte: self.dataOrOffset = cast[int32](v)
  elif T is GffChar: self.dataOrOffset = cast[int32](v)
  elif T is GffWord: self.dataOrOffset = cast[int32](v)
  elif T is GffShort: self.dataOrOffset = cast[int32](v)
  elif T is GffDword: self.dataOrOffset = cast[int32](v)
  elif T is GffInt: self.dataOrOffset = cast[int32](v)
  elif T is GffFloat: self.dataOrOffset = cast[int32](v)

  elif T is GffDword64: self.gffDword64 = v
  elif T is GffInt64: self.gffInt64 = v
  elif T is GffDouble: self.gffDouble = v
  elif T is GffCExoString: self.gffCExoString = v
  elif T is GffResRef: self.gffResRef = v
  elif T is GffCExoLocString: self.gffCExoLocString = v
  elif T is GffList: self.gffList = v
  elif T is GffStruct: self.gffStruct = v
  elif T is GffVoid: self.gffVoid = v
  else: {.fatal: "You added a gff type but didn't make it resolvable in assignValue()"}

proc newGffList*(): GffList = newSeq[GffStruct]()

proc newCExoLocString*(): GffCExoLocString =
  new(result)
  result.entries = newTable[int, string]()
  result.strRef = BadStrRef

proc initGffStruct(self: GffStruct, id: int32 = -1) =
  self.fields = newOrderedTable[string, GffField]()
  self.id = id

proc newGffStruct*(id: int32 = -1): GffStruct =
  new(result)
  initGffStruct(result, id)

proc newGffRoot*(f = "GFF "): GffRoot =
  result = GffRoot()
  initGffStruct(result)
  result.fileVersion = "V3.2"
  result.fileType = f

proc newGffField*[T : GffFieldType](gen: typedesc[T], value: T): GffField =
  result = GffField(resolved: true, fieldKind: typeDescToKind(gen))
  result.assignValue(value)

proc get*[T: GffFieldType](self: GffStruct, label: string, t: typedesc[T]): T =
  ## Returns the typed field value.
  ## Will raise a GffError if the field cannot be retrieved.
  let fld = self.fields[label]
  ensureTypeOf(fld.fieldKind, t)
  result = fld.getValue(t)

proc getOrDefault*[T: GffFieldType](self: GffStruct, label: string, default: T): T =
  ## Attempts to retrieve a field value of the given type. Will return `default`
  ## if the field is missing or the type mismatches.
  result = if self.hasField(label, T): self[label, T] else: default

proc `[]`*[T: GffFieldType](self: GffStruct, label: string, t: typedesc[T]): T =
  ## Alias for get.
  result = get(self, label, t)

proc `[]`*[T: GffFieldType](self: GffStruct, label: string, default: T): T =
  ## Alias for getOrDefault.
  result = getOrDefault(self, label, default)

proc putValue*[T: GffFieldType](self: GffStruct, label: string, t: typedesc[T], value: T) =
  ## Assigns a new gff field to the given struct under label. The old value will
  ## be discarded.
  expect(label.len > 0 and label.len <= 16)
  self.fields[label] = newGffField(T, value)
  self.fields[label].struct = self

proc `[]=`*[T: GffFieldType](self: GffStruct, label: string, t: typedesc[T], value: T) =
  ## Alias for putField.
  putValue(self, label, t, value)

proc del*(self: GffStruct, label: string) =
  ## Removes a label from this struct.
  self.fields.del(label)

proc hasField*[T: GffFieldType](self: GffStruct, label: string, t: typedesc[T]): bool =
  ## Returns true if self has a label of the given type.
  result = self.fields.hasKey(label) and self.fields[label].fieldKind.hasTypeOf(t)

proc fields*(self: GffStruct): OrderedTableRef[string, GffField] =
  ## Gives you a ref to the table holding all the GffField instannces.
  ## You can use this to iterate or do advanced manipulation if the
  ## strongly typed syntactic sugar on GffStruct is not enough.
  result = self.fields

proc readStructInto(loader: GffLazyLoader, idx: int, into: GffStruct) =
  ## Reads a gff struct. Part of the lazyloader.

  doAssert(loader.root != nil)
  into.root = loader.root

  expect(idx >= 0 and idx < loader.structs.len)

  let structData = loader.structs[idx]
  expect(not structData.resolved, "struct idx referenced multiple times")
  loader.structs[idx].resolved = true

  var fieldArrayIndices: seq[int]
  # If Struct.FieldCount = 1, this is an index into the Field Array
  if structData.fieldCount == 1:
    fieldArrayIndices = @[structData.dataOrOffset]

  # If Struct.FieldCount > 1, this is a byte offset into the Field Indices array,
  # where there is an array of DWORDs having a number of elements equal to
  # Struct.FieldCount. Each one of these DWORDs is an index into the Field Array.
  else:
    # Byte offset! -> div 4
    let offsetStart = structData.dataOrOffset div 4
    let offsetEnd = offsetStart + structData.fieldCount

    expect(offsetStart >= 0 and offsetStart < loader.fieldIndices.len)
    expect(offsetEnd >= 0 and offsetEnd <= loader.fieldIndices.len)
    expect(offsetStart <= offsetEnd)

    fieldArrayIndices = loader.fieldIndices[offsetStart..<offsetEnd]


  let labelArrayIndices = fieldArrayIndices.map(proc (it: int): int =
    expect(it >= 0 and it < loader.fieldsArray.len)
    result = loader.fieldsArray[it].labelIndex
  )

  expect(labelArrayIndices.len == fieldArrayIndices.len)

  into.id = structData.id

  # Now stub out all fields.
  for idx in 0..<labelArrayIndices.len:
    let lblidx = labelArrayIndices[idx]
    expect(lblidx >= 0 and lblidx < loader.labelsArray.len)
    let lbl = loader.labelsArray[lblidx]
    expect(not into.fields.hasKey(lbl))

    let faix = fieldArrayIndices[idx]
    expect(not loader.fieldsArray[faix].resolved, "field index referenced twice")
    loader.fieldsArray[faix].resolved = true

    let fld = GffField(
      struct: into,
      resolved: false,
      fieldKind: loader.fieldsArray[faix].fieldKind.GffFieldKind,
      dataOrOffset: loader.fieldsArray[faix].dataOrOffset
    )

    let isC = isComplexType(fld.fieldKind)

    # always resolve direct types; we already have the data, since that's really
    # just casts.
    if not isC or not loader.lazy:
      try: discard fld.resolve()
      except IOError: raise newException(IOError, getCurrentExceptionMsg() &
        " while immediate-loading field " & lbl & " of type " & $fld.fieldKind &
        " on struct " & $into.id)

    into.fields[lbl] = fld

proc resolve(self: GffField): GffField =
  ## Resolves a field; this is part of the lazyloading mechanism.
  result = self
  if self.resolved: return

  doAssert(self.struct != nil)
  doAssert(self.struct.root != nil)
  doAssert(self.struct.root.loader != nil)
  let loader = self.struct.root.loader

  # Complex types store their field data in the big-bad data array. Because
  # I'm a cheap hack, I'm just storing the io and seeking on demand.
  if isComplexType(self.fieldKind) and
      self.fieldKind != GffFieldKind.Struct and
      self.fieldKind != GffFieldKind.List:

    expect(self.dataOrOffset >= 0 and self.dataOrOffset <
      loader.header.fieldDatasize)

    let np = loader.ioStartPosition +
      loader.header.fieldDataOffset +
      self.dataOrOffset
    loader.io.setPosition(np)

  case self.fieldKind

  # simple types: handled above
  of GffFieldKind.Byte: discard
  of GffFieldKind.Char: discard
  of GffFieldKind.Word: discard
  of GffFieldKind.Short: discard
  of GffFieldKind.Dword: discard
  of GffFieldKind.Int: discard
  of GffFieldKind.Float: discard

  # complex types
  of GffFieldKind.CExoLocString:
    # self.gffCExoLocString = extractField(GffCExoLocString, tmp, fie)
    self.gffCExoLocString = newCExoLocString()
    let totalSz = loader.io.readInt32()
    let previousIoPosition = loader.io.getPosition
    self.gffCExoLocString.strRef = loader.io.readUInt32()
    let count = loader.io.readInt32()
    for i in 0..<count:
      let exoId = loader.io.readInt32()
      let strSz = loader.io.readInt32()
      self.gffCExoLocString.entries[exoId] =
        loader.io.readStrOrErr(strSz).fromNwnEncoding
    doAssert(totalSz == loader.io.getPosition - previousIoPosition)


  of GffFieldKind.Dword64: self.gffDword64 = cast[uint64](loader.io.readInt64())
  of GffFieldKind.Int64: self.gffInt64 = cast[int64](loader.io.readInt64())
  of GffFieldKind.Double: self.gffDouble = cast[float64](loader.io.readFloat64())

  of GffFieldKind.CExoString: self.gffCExoString =
    loader.io.readStrOrErr(loader.io.readInt32()).fromNwnEncoding

  of GffFieldKind.ResRef: self.gffResRef =
    loader.io.readStrOrErr(loader.io.readInt8()).GffResRef

  of GffFieldKind.Void: self.gffVoid =
    loader.io.readStrOrErr(loader.io.readInt32()).GffVoid

  of GffFieldKind.Struct:
    self.gffStruct = newGffStruct()
    readStructInto(loader, self.dataOrOffset, self.gffStruct)

  of GffFieldKind.List:
    let offset = self.dataOrOffset div sizeof(int32)

    # # The first DWORD is the Size of the List, and it specifies how many Struct
    # # elements the List contains.
    expect(offset >= 0 and offset < loader.listIndices.len)
    let listSz = loader.listIndices[offset]

    # # There are Size DWORDS after that, each one an index into the Struct Array
    let offsetStart = offset + 1 # 1 = skip list size we just read above
    let offsetEnd = offsetStart + listSz

    expect(offsetStart >= 0 and offsetStart <= loader.listIndices.len)
    expect(offsetEnd >= offsetStart and offsetEnd <= loader.listIndices.len)

    let list = loader.listIndices[offsetStart..<offsetEnd]

    self.gffList = list.map(proc (idx: int): GffStruct =
      result = newGffStruct()
      readStructInto(loader, idx, result)
    )

  self.resolved = true

proc readGffRoot*(fromIO: Stream, lazyLoad: bool = true): GffRoot =
  ## Reads a gff from a IO.
  ## This assumes that `fromIO` is positioned at the beginning of the gff header,
  ## including the magic bytes ("BIC V3.2")

  result = newGffRoot()
  new(result.loader)

  result.loader.root = result
  result.loader.lazy = lazyload
  result.loader.io = fromIO
  result.loader.ioStartPosition = fromIO.getPosition
  let ip = result.loader.ioStartPosition

  result.fileType = fromIO.readStrOrErr(4)
  result.fileVersion = fromIO.readStrOrErr(4)

  expect("V3.2" == result.fileVersion)
  expect(result.fileType.len == 4)

  let header = (
    structOffset:       fromIo.readInt32().int32,
    structCount:        fromIo.readInt32().int32,
    fieldOffset:        fromIo.readInt32().int32,
    fieldCount:         fromIo.readInt32().int32,
    labelOffset:        fromIo.readInt32().int32,
    labelCount:         fromIo.readInt32().int32,
    fieldDataOffset:    fromIo.readInt32().int32,
    fieldDataSize:      fromIo.readInt32().int32,
    fieldIndicesOffset: fromIo.readInt32().int32,
    fieldIndicesSize:   fromIo.readInt32().int32,
    listIndicesOffset:  fromIo.readInt32().int32,
    listIndicesSize:    fromIo.readInt32().int32
  )
  result.loader.header = header

  expect(header.structOffset == 56)

  # Unpack all labels to their strings so we can reference them.
  fromIO.setPosition(ip + header.labelOffset)
  result.loader.labelsArray = toSeq(countup(0, header.labelCount - 1)).
    map(_ => fromIO.readStrOrErr(16).strip(false, true, {'\0'}))

  # Unpack fields (type, lblidx, dataOrOffset)
  fromIO.setPosition(ip + header.fieldOffset)
  result.loader.fieldsArray = newSeq[GffRootFieldEntry]()
  for i in 0..<header.fieldCount:
    let kind = fromIO.readInt32().int
    expect(kind >= GffFieldKind.low.int and kind <= GffFieldKind.high.int)
    let labelIdx = fromIO.readInt32().int
    let dataOrOffset = fromIO.readInt32().int
    result.loader.fieldsArray.add((kind.GffFieldKind, labelIdx, dataOrOffset, false).GffRootFieldEntry)

  expect(result.loader.fieldsArray.len == header.fieldCount)

  # unpack field indices
  fromIO.setPosition(ip + header.fieldIndicesOffset)
  result.loader.fieldIndices = toSeq(countup(0, header.fieldIndicesSize div 4 - 1)).
    map(_ => fromIO.readInt32().int)

  # list indices
  fromIO.setPosition(ip + header.listIndicesOffset)
  result.loader.listIndices = toSeq(countup(0, header.listIndicesSize div 4 - 1)).
    map(_ => fromIO.readInt32().int)

  # structs (id, dataOrOffset, fieldCount)
  fromIO.setPosition(ip + header.structOffset)
  result.loader.structs = toSeq(countup(0, header.structCount - 1)).
    map(_ => (fromIO.readInt32(), fromIO.readInt32().int, fromIO.readInt32().int, false).GffRootStructEntry)

  readStructInto(result.loader, 0, result)

  if not lazyload: # we're done loading, free up the loader.
    result.loader.root = nil
    result.loader = nil

proc write*(io: Stream, root: GffRoot) =
  # Write a gff root object to a stream!

  expect(root.fileType.len == 4)
  expect(root.fileVersion.len == 4)
  expect(root.id == -1)

  var labels = newSeq[string]()
  var structs = newSeq[GffRootStructEntry]()
  var fields = newSeq[GffRootFieldEntry]()
  var fieldData = newStringStream()
  var fieldIndicesArray = newSeq[int]()
  var listIndicesArray = newSeq[int]()

  proc collector(s: GffStruct): int =
    var thisStructFieldIds = newSeq[int]()

    var thisStruct: GffRootStructEntry
    thisStruct.id = s.id

    # reserve our spot in the seen-first struct array, since we are recursing
    # depth-first
    structs.add(thisStruct)
    result = structs.len - 1

    for k, v in pairs(s.fields):
      var labelPos = labels.find(k)
      if labelPos == -1: labels.add(k); labelPos = labels.len - 1
      var f: GffRootFieldEntry
      f.labelIndex = labelPos
      f.fieldKind = v.fieldKind

      if not isComplexType(v.fieldKind):
        f.dataOrOffset = v.dataOrOffset

      else:
        f.dataOrOffset = fieldData.getPosition

        case v.fieldKind:

        # simple types: handled above
        of GffFieldKind.Byte: discard
        of GffFieldKind.Char: discard
        of GffFieldKind.Word: discard
        of GffFieldKind.Short: discard
        of GffFieldKind.Dword: discard
        of GffFieldKind.Int: discard
        of GffFieldKind.Float: discard

        of GffFieldKind.CExoString:
          let s = v.getValue(GffCExoString).toNwnEncoding
          fieldData.write(s.len.int32)
          fieldData.write(s)
          assert(fieldData.getPosition == f.dataOrOffset + 4 + s.len)

        of GffFieldKind.ResRef:
          let s = v.getValue(GffResRef).string
          fieldData.write(s.len.int8)
          fieldData.write(s)
          assert(fieldData.getPosition == f.dataOrOffset + 1 + s.len)

        of GffFieldKind.CExoLocString:
          let s = v.getValue(GffCExoLocString)
          let m = newStringStream()
          for lang, str in pairs(s.entries):
            let str2 = str.toNwnEncoding
            m.write(lang.int32)
            m.write(str2.len.int32)
            m.write(str2)
          # +8: A CExoLocString begins with a single DWORD (4-byte unsigned integer)
          #     which stores the total number of bytes in the CExoLocString,
          #     not including the first 4 size bytes.
          fieldData.write((m.getPosition + 8).int32) # totalsz
          fieldData.write(s.strref.StrRef)
          fieldData.write(s.entries.len.int32) #strcount
          m.setPosition(0)
          fieldData.write(m.readAll)

        of GffFieldKind.Struct:
          let structIdx = collector(v.getValue(GffStruct))
          f.dataOrOffset = structIdx

        of GffFieldKind.List:
          # this shit doesnt work yet
          var thisListStructIdxs = newSeq[int]()
          for li in v.getValue(GffList):
            thisListStructIdxs.add(collector(li))

          f.dataOrOffset = listIndicesArray.len * 4
          listIndicesArray.add(thisListStructIdxs.len)
          listIndicesArray &= thisListStructIdxs

        of GffFieldKind.Dword64:
          fieldData.write(v.getValue(GffDword64))

        of GffFieldKind.Int64:
          fieldData.write(v.getValue(GffInt64))

        of GffFieldKind.Double:
          fieldData.write(v.getValue(GffDouble))

        of GffFieldKind.Void:
          let vd = v.getValue(GffVoid).string
          fieldData.write(uint32 vd.len)
          fieldData.write(vd)

      let fieldPos = fields.len
      fields.add(f)
      thisStructFieldIds.add(fieldPos)

    thisStruct.fieldCount = thisStructFieldIds.len
    if thisStruct.fieldCount == 1: thisStruct.dataOrOffset = thisStructFieldIds[0]
    elif thisStruct.fieldCount > 1:
      thisStruct.dataOrOffset = fieldIndicesArray.len * 4
      fieldIndicesArray &= thisStructFieldIds

     # since it's by-value, reassign after we are done with it
    structs[result] = thisStruct

  let rootStructIdx = collector(root)
  doAssert(rootStructIdx == 0, "the gff root struct should always be 0")

  let ioPosStart = io.getPosition

  io.write(root.fileType)
  io.write(root.fileVersion)
  assert(io.getPosition == 8 + ioPosStart)

  var offset = 56

  # structOffset:
  io.write(int32(offset))
  # structCount:
  io.write(int32(structs.len))
  offset += structs.len * 12

  # fieldOffset:
  io.write(int32(offset))
  # fieldCount:
  io.write(int32(fields.len))
  offset += fields.len * 12

  # labelOffset:
  io.write(int32(offset))
  # labelCount:
  io.write(int32(labels.len))
  offset += labels.len * 16

  # for complex types!
  # fieldDataOffset:
  io.write(int32(offset))
  # fieldDataSize:
  io.write(int32(fieldData.getPosition))
  offset += fieldData.getPosition

  # fieldIndicesOffset:
  io.write(int32(offset))
  # fieldIndicesSize: # num of bytes
  io.write(int32(fieldIndicesArray.len * 4))
  offset += fieldIndicesArray.len * 4

  # listIndicesOffset:
  io.write(int32(offset))
  # listIndicesSize:
  io.write(int32(listIndicesArray.len * 4))
  offset += listIndicesArray.len * 4

  assert(io.getPosition == ioPosStart + 56)

  # structs are written out as (id, dataOrOffset, fieldCount)
  assert(structs.len > 0)
  assert(structs[0].id == -1)
  for s in structs:
    io.write(s.id.int32)
    io.write(s.dataOrOffset.int32)
    io.write(s.fieldCount.int32)
  assert(io.getPosition == ioPosStart + 56 + structs.len * 12)

  # fields are written out as (kind, labelIdx, dataOrOffset)
  for f in fields:
    io.write(f.fieldKind.int32)
    io.write(f.labelIndex.int32)
    io.write(f.dataOrOffset.int32)
  assert(io.getPosition == ioPosStart + 56 + structs.len * 12 + fields.len * 12)

  # Labels are written in 16-byte chunks, filled with null bytes.
  for l in labels: io.write(l & repeat(0.char, 16 - l.len))
  assert(io.getPosition == ioPosStart + 56 + structs.len * 12 + fields.len * 12 +
    labels.len * 16)

  # fieldData
  fieldData.setPosition(0)
  io.write(fieldData.readAll)
  assert(io.getPosition == ioPosStart + 56 + structs.len * 12 + fields.len * 12 +
    labels.len * 16 + fieldData.getPosition)

  # field indices
  for i in items(fieldIndicesArray): io.write(i.int32)
  assert(io.getPosition == ioPosStart + 56 + structs.len * 12 + fields.len * 12 +
    labels.len * 16 + fieldData.getPosition + fieldIndicesArray.len * 4)

  # list indices (list.dataOrOffset -> sz + structids)
  for i in items(listIndicesArray): io.write(i.int32)
  assert(io.getPosition == ioPosStart + 56 + structs.len * 12 + fields.len * 12 +
    labels.len * 16 + fieldData.getPosition + fieldIndicesArray.len * 4 +
    listIndicesArray.len * 4)

proc `$`*[T: GffResRef | GffVoid](x: T): string = x.string
proc `$`*(x: GffStruct): string =
  if x of GffRoot: "<GffRoot: " & x.GffRoot.fileType & x.GffRoot.fileVersion & ">"
  else: "<GffStruct " & $x.fields.len & " fields>"
proc `$`*(x: GffField): string =
  result = "<Gff" & $x.fieldKind
  # result &= " " & x.label

  case x.fieldKind:
    of GffFieldKind.Byte: result &= "=" & $x.getValue(GffByte)
    of GffFieldKind.Char: result &= "=" & $x.getValue(GffChar)
    of GffFieldKind.Word: result &= "=" & $x.getValue(GffWord)
    of GffFieldKind.Short: result &= "=" & $x.getValue(GffShort)
    of GffFieldKind.Dword: result &= "=" & $x.getValue(GffDword)
    of GffFieldKind.Int: result &= "=" & $x.getValue(GffInt)
    of GffFieldKind.Float: result &= "=" & $x.getValue(GffFloat)

    of GffFieldKind.Dword64: result &= "=" & $x.gffDword64
    of GffFieldKind.Int64: result &= "=" & $x.gffInt64
    of GffFieldKind.Double: result &= "=" & $x.gffDouble
    of GffFieldKind.CExoString: result &= "=" & $x.gffCExoString
    of GffFieldKind.ResRef: result &= "=" & $x.gffResRef
    of GffFieldKind.CExoLocString: result &= "=" & $x.gffCExoLocString.entries

    else: discard
    # of GffFieldKind.Void: result &= "=" & $x.gffVoid
    # of GffFieldKind.Struct: result &= "=" & $x.gffStruct
    # of GffFieldKind.List: result &= "=" & $x.gffList

  result &= ">"
