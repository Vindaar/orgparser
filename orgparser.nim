import std/[strutils, sequtils, tables, options, algorithm, strformat]

type
  TokenKind* = enum
    tkNone,
    tkText, # arbitrary text
    tkNewline, # `\n`
    tkStar, # `*`
    tkColon, # `:`
    tkEqual, # `=`
    tkTilde, # `~`
    tkUnderscore, # `_`
    tkBracketOpen, # `[`
    tkBracketClose, # `]`
    tkIdent  # single word e.g. between `*foo*`, `=bar=`, ...

  OrgNodeKind* = enum
    ogSection, # `* Foo \n some text \n` until next section
    ogProperty, # `:Foo: Bar`
    ogPropertyStart, # `:PROPERTIES:` Wont appear in final `OrgNode` object, only for parsing
    ogPropertyEnd, # `:END`: Wont appear in final `OrgNode` object, only for parsing
    ogPropertyList, # `:PROPERTIES:\n multiple ogProperty\n:END:`
    ogBold, # `*foo*`
    ogUnderlined, # `_foo_`
    ogLink, # `[[https://...][foo]]`
    ogMonospace, # `~foo~` or `=foo=`
    ogText # Any text that is not any of the above

  Token* = object
    kind*: TokenKind
    value*: string

proc tokenize*(org: string): seq[Token] =
  var i = 0
  var lastWasOp = false
  template addToken(tk): untyped =
    if buf.len > 0:
      result.add Token(kind: tkText, value: buf)
      buf.setLen(0)
      doAssert identBuf.len == 0, "Ident buffer not empty! `" & $identBuf & "`"
    elif identBuf.len > 0: # found identifier between two operators
      result.add Token(kind: tkIdent, value: identBuf)
      identBuf.setLen(0)
      doAssert buf.len == 0, "Buffer was not empty! " & $buf
    result.add Token(kind: tk)
    lastWasOp = true
  var buf = newStringOfCap(128_000)
  var identBuf = newStringOfCap(512)
  while i < org.len:
    case org[i]
    of '*': addToken tkStar
    of '[': addToken tkBracketOpen
    of ']': addToken tkBracketClose
    of '_': addToken tkUnderscore
    of '=': addToken tkEqual
    of '~': addToken tkTilde
    of ':': addToken tkColon
    of '\n': addToken tkNewline
    of ' ':
      if identBuf.len > 0: # not an identifier after all, add to buf, reset
        buf.add identBuf
        identBuf.setLen(0)
      buf.add org[i]
      lastWasOp = false
    else:
      if lastWasOp:
        identBuf.add org[i]
      else:
        buf.add org[i]
    inc i

type
  OrgObj* = object
    case kind*: OrgNodeKind
    of ogText:
      text*: string
    of ogSection:
      sec*: Section # `* Foo \n some text \n` until next section
    of ogPropertyList:
      propList*: PropertyList # `:PROPERTIES:\n multiple ogProperty\n:END:`
    of ogProperty:
      prop*: Property # `:Foo: Bar`
    of ogBold, ogUnderlined, ogMonospace:
      emph*: string
    of ogLink:
      link*: Link # `[[https://...][foo]]`
    of ogPropertyStart, ogPropertyEnd: discard # no fields needed
  OrgNode* = ref OrgObj

  Property* = object
    key*: string
    value*: seq[OrgNode]

  PropertyList* = seq[Property]

  Link* = object
    link*: string
    desc*: string

  Section* = object
    title*: seq[OrgNode]
    level*: int # The section level (number of `*`)
    body*: seq[OrgNode]

proc parseToken*(token: Token, tokens: var seq[Token]): OrgNode
func tryPop(tokens: var seq[Token]): Token =
  if tokens.len > 0: # return `tkNone` otherwise
    result = tokens.pop()

func tryPeek(tokens: var seq[Token]): Token =
  if tokens.len > 0: # return `tkNone` otherwise
    result = tokens[^1]

func serialize(t: Token): string =
  case t.kind
  of tkText: result = t.value
  of tkNewline: result = "\n"
  of tkStar: result = "*"
  of tkColon: result = ":"
  of tkEqual: result = "="
  of tkTilde: result = "~"
  of tkUnderscore: result = "_"
  of tkBracketOpen: result = "["
  of tkBracketClose: result = "]"
  of tkIdent: result = t.value
  of tkNone: result = ""

func `$`(t: Token): string = t.serialize()
func `$`(org: OrgNode): string
func `$`(org: seq[OrgNode]): string

proc `$`(uri: Link): string =
  result = &"[[{uri.link}]"
  if uri.desc.len > 0:
    result.add &"[uri.desc]]"
  else:
    result.add "]"

proc `$`(p: Property): string =
  result = &":{p.key}:{p.value}"

proc `$`(pList: PropertyList): string =
  result = ":PROPERTIES:\n"
  for p in pList:
    result.add $p
  result.add ":END:\n"

proc serialize(org: OrgNode): string =
  case org.kind
  of ogSection:
    result = repeat('*', org.sec.level)
    result.add $org.sec.title
    result.add $org.sec.body
  of ogProperty:      result = $org.prop
  of ogPropertyStart: result = ":PROPERTIES:"
  of ogPropertyEnd:   result = ":END:"
  of ogPropertyList:  result = $org.propList
  of ogBold:          result = &"*{org.emph}*"
  of ogUnderlined:    result = &"_{org.emph}_"
  of ogLink:          result = $org.link
  of ogMonospace:     result = &"~{org.emph}~"
  of ogText:          result = org.text

func `$`(org: OrgNode): string = serialize(org)
func `$`(org: seq[OrgNode]): string =
  for x in org:
    result.add serialize(x)

proc tryParseOperator(t: Token, tokens: var seq[Token], tkKind: TokenKind): (bool, string) =
  doAssert t.kind == tkKind, "No: " & $t
  let tn  = tokens.tryPop() # try next token
  let tnn = tokens.tryPeek() # only peek next next token
  if tn.kind != tkNone and tn.kind == tkIdent and
     tnn.kind != tkNone and tnn.kind == tkKind:
    discard tokens.pop() # pop `tnn` to actually consume
    result = (true, tn.value)
  else: # concat to a text node. In this case `tnn` should remain where it is!
    result = (false, $t & $tn)

proc tryParseOperator(t: Token, tokens: var seq[Token], tkKind: TokenKind, ogKind: static OrgNodeKind): OrgNode =
  let (success, val) = tryParseOperator(t, tokens, tkKind)
  if success:
    result = OrgNode(kind: ogKind, emph: val)
  else:
    result = OrgNode(kind: ogText, text: val)

proc tryParseProperty(t: Token, tokens: var seq[Token]): OrgNode =
  ## Parses either a property or a full property list
  doAssert t.kind == tkColon
  let (success, key) = tryParseOperator(t, tokens, tkColon)
  if success and key == "PROPERTIES": # parse property list
    # parse until `:END:` found
    var tn: Token
    var o: OrgNode = OrgNode(kind: ogText) # placeholder
    var pList: PropertyList
    while tokens.len > 0 and o.kind != ogPropertyEnd:
      tn = tokens.pop()
      if tn.kind == tkNewline: continue # ignore newlines
      o = parseToken(tn, tokens)
      doAssert o.kind in {ogProperty, ogPropertyEnd}, "No was: " & $o & " of kind: " & $o.kind
      if o.kind != ogPropertyEnd:
        pList.add o.prop
    result = OrgNode(kind: ogPropertyList, propList: pList)
  elif success and key == "END":
    result = OrgNode(kind: ogPropertyEnd)
  elif success: # parse individual property
    var tok = Token(kind: tkNone) #  = tokens.tryPop()
    var val: seq[OrgNode]
    var j = tokens.high
    for i in 0 ..< 5:
      dec j
    while tokens.len > 0 and tok.kind != tkNewline:
      tok = tokens.pop()
      val.add parseToken(tok, tokens)
    result = OrgNode(kind: ogProperty, prop: Property(key: key, value: val))
  else: # neither, just a colon
    result = OrgNode(kind: ogText, text: $t & key)

proc tryParseLink(t: Token, tokens: var seq[Token]): (bool, string) =
  ## Separte from `tryParseOperator`, because the arguments of
  ## the actual link nor description are a simple ident.
  let tn  = tokens.tryPop() # try next token
  let tnn = tokens.tryPop() # try next next token
  if tn.kind != tkNone and tn.kind == tkText and
     tnn.kind != tkNone and tnn.kind == tkBracketClose:
    result = (true, tn.value)
  else: # concat to a text node
    result = (false, $t & $tn & $tnn)

proc parseLink(t: Token, tokens: var seq[Token]): OrgNode =
  doAssert t.kind == tkBracketOpen
  let (success, link) = tryParseLink(t, tokens)
  if success:
    # check if it has an description
    let tn = tokens.pop()
    var uri: Link
    case tn.kind
    of tkBracketClose: uri = Link(link: link) # just a single link
    of tkBracketOpen: # has description
      let (sd, desc) = tryParseLink(tn, tokens)
      doAssert sd, "Failed to parse: " & $desc
      uri = Link(link: link, desc: desc)
    else: raiseAssert "Unsupported here " & $tn
    result = OrgNode(kind: ogLink, link: uri)

proc sectionUpcoming(tokens: seq[Token]): bool =
  ## Determines if a section is upcoming next
  ## The pattern is:
  ## - newline
  ## - one or more `*`
  ## - space
  ## - any regular node except `tkIdent` (would imply no space after `*`)
  ## - newline
  template returnIfNot(tk): untyped =
    if i < 0 or tokens[i].kind != tk: return false
  template returnIf(tk): untyped =
    if i < 0 or tokens[i].kind == tk: return false

  var i = tokens.high
  returnIfNot(tkNewline)
  dec i
  returnIfNot(tkStar)
  while i > 0 and tokens[i].kind == tkStar:
    dec i
  returnIf(tkIdent)
  dec i
  result = true

proc determineSectionLevel(t: Token, tokens: seq[Token]): int =
  if t.kind == tkStar:
    result = 1
    var i = tokens.high
    while i > 0 and tokens[i].kind == tkStar:
      inc result
      dec i

proc parseTitle(t: Token, tokens: var seq[Token]): seq[OrgNode] =
  ## Parses a title from a section. Must only be called if `t` is not an
  ## ident and the previous token is a `tkStar`.
  doAssert t.kind != tkIdent, "Ident cannot be valid title" & $t
  if t.kind == tkNewline: return
  else:
    var tok: Token
    while tokens.len > 0 and tok.kind != tkNewline: # every token to next newline is part of the title
      tok = tokens.pop()
      if tok.kind == tkStar: continue # skip, already taken into account by `determineSectionLevel`
      result.add tok.parseToken(tokens)

proc parseOperatorOrSection(t: Token, tokens: var seq[Token]): OrgNode =
  let level = determineSectionLevel(t, tokens)
  if tokens.len >= 2:
    let tn = tokens[^1] # only peek!
    if tn.kind == tkIdent: # should be an operator
      result = tryParseOperator(t, tokens, tkStar, ogBold)
    else: # should be a section
      var body: seq[OrgNode]
      let title = parseTitle(tn, tokens)
      while tokens.len > 0 and not sectionUpcoming(tokens):
        let tn = tokens.pop()
        body.add tn.parseToken(tokens)
      result = OrgNode(kind: ogSection, sec: Section(title: title, level: level, body: body))
  elif tokens.len == 1: # `* FooEOF`
    let tn = tokens.pop()
    let title = parseTitle(tn, tokens)
    result = OrgNode(kind: ogSection, sec: Section(title: title, level: level))
  else:
    raiseAssert "Invalid branch"

proc parseToken*(token: Token, tokens: var seq[Token]): OrgNode =
  case token.kind
  of tkText: result = OrgNode(kind: ogText, text: token.value) # arbitrary text
  of tkNewline: result = OrgNode(kind: ogText, text: "\n")
  of tkStar: result = parseOperatorOrSection(token, tokens) # `*`
  of tkEqual: result = tryParseOperator(token, tokens, tkEqual, ogMonospace) # `=`
  of tkTilde: result = tryParseOperator(token, tokens, tkTilde, ogMonospace) # `~`
  of tkUnderscore: result = tryParseOperator(token, tokens, tkUnderscore, ogUnderlined) # `_`
  of tkColon: result = tryParseProperty(token, tokens) # `:`
  of tkBracketOpen: result = parseLink(token, tokens) # `[`
  of tkBracketClose: raiseAssert "Closing `]`" # ??? ]
  of tkIdent: result = OrgNode(kind: ogText, text: token.value) #raiseAssert "Ident: " & $token # single word e.g. between `*foo*`, `=bar=`, ...
  of tkNone: raiseAssert "Not possible"

proc parse*(tokens: seq[Token]): seq[OrgNode] =
  ## `Tokens` is in reverse order. That way we can just `pop`.
  var tokens = tokens.reversed()
  while tokens.len > 0:
    let t = tokens.pop()
    echo "Got token: ", t
    ## XXX: concat two text nodes if they appear after another!
    result.add t.parseToken(tokens)

when isMainModule:
  const path = "/home/basti/org/Documents/CV_data.org"
  let data = readFile(path)

  var toks = tokenize(data)
  echo "Tokens: ", toks

  let org = parse(toks)

  echo org
