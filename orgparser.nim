import std/[strutils, sequtils, tables, options, algorithm, strformat, options]

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
    ogNone, # empty node
    ogArray, # just a seq of successive nodes, e.g. text with syntax highlighting or similar
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
    line*: int
    column*: int

  OrgObj* = object
    case kind*: OrgNodeKind
    of ogArray:
      children*: seq[OrgNode]
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
    of ogPropertyStart, ogPropertyEnd, ogNone: discard # no fields needed
  OrgNode* = ref OrgObj

  Property* = object
    key*: string
    value*: OrgNode

  PropertyList* = seq[Property]

  Link* = object
    link*: string
    desc*: string

  Section* = object
    title*: OrgNode
    tags*: seq[string] # All tags on the section title
    level*: int # The section level (number of `*`)
    body*: OrgNode

proc newOrgEmpty*(): OrgNode = OrgNode(kind: ogNone )
proc newOrgArray*(): OrgNode = OrgNode(kind: ogArray)

proc add*(org: OrgNode, val: OrgNode) =
  ## Adds the `val` to the `OrgNode` of kind `ogArray`
  doAssert org.kind == ogArray, "Must be an array, but is: " & $org.kind
  org.children.add val

iterator items*(org: OrgNode): OrgNode =
  ## Yields all elements in the given Org array
  doAssert org.kind == ogArray, "Must be an array, but is: " & $org.kind
  for el in org.children:
    yield el

proc tokenize*(org: string): seq[Token] =
  var i = 0
  var lastWasOp = false
  var line, col = 1 ## NOTE: The column and line counters start at 1!
  template addToken(tk): untyped =
    if not inComment:
      if buf.len > 0:
        result.add Token(kind: tkText, value: buf, line: line, column: col)
        buf.setLen(0)
        doAssert identBuf.len == 0, "Ident buffer not empty! `" & $identBuf & "`"
      elif identBuf.len > 0: # found identifier between two operators
        result.add Token(kind: tkIdent, value: identBuf, line: line, column: col)
        identBuf.setLen(0)
        doAssert buf.len == 0, "Buffer was not empty! " & $buf
      result.add Token(kind: tk, line: line, column: col)
      lastWasOp = true

  template addChar(): untyped =
    if not inComment:
      if lastWasOp:
        identBuf.add org[i]
      else:
        buf.add org[i]

  var buf = newStringOfCap(128_000)
  var identBuf = newStringOfCap(512)
  var inComment = false
  while i < org.len:
    case org[i]
    of '*':  addToken tkStar
    of '[':  addToken tkBracketOpen
    of ']':  addToken tkBracketClose
    of '=':  addToken tkEqual
    of '~':  addToken tkTilde
    of ':':  addToken tkColon
    of '#':
      if col == 1: inComment = true
      else: addChar()
    of '+':
      if col == 2: inComment = false
      else: addChar()
    of '_':
      if not inComment:
        if identBuf.len > 0 and i < org.len - 1 and org[i+1] notin {'*','[',']','=','~',':',' ','\n'}:
          # just a regular underscore in an identifier, e.g. `CUSTOM_ID`
          identBuf.add org[i]
        else:
          addToken tkUnderscore
    of '\n': addToken tkNewline; inc line; col = 0; inComment = false
    of ' ':
      if not inComment:
        if identBuf.len > 0: # not an identifier after all, add to buf, reset
          buf.add identBuf
          identBuf.setLen(0)
        buf.add org[i]
        lastWasOp = false
    else:
      addChar()
    inc i
    inc col

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

func `$`*(t: Token): string = t.serialize()
func `$`*(org: OrgNode): string

proc `$`*(uri: Link): string =
  result = &"[[{uri.link}]"
  if uri.desc.len > 0:
    result.add &"[uri.desc]]"
  else:
    result.add "]"

proc `$`*(p: Property): string =
  result = &":{p.key}: {p.value}"

proc `$`*(pList: PropertyList): string =
  result = ":PROPERTIES:\n"
  for p in pList:
    result.add $p & "\n"
  result.add ":END:\n"

proc tagsAsString(s: seq[string]): string =
  ## Given a seq of tags, returns them as an Org tag string
  result = ":" & s.join(":") & ":"

proc serialize(org: OrgNode): string =
  if org.isNil: return "OrgNode(nil)"
  case org.kind
  of ogArray:
    for x in org.children:
      result.add serialize(x)
  of ogSection:
    result = repeat('*', org.sec.level) & " "
    result.add $org.sec.title & repeat(' ', 20) & tagsAsString(org.sec.tags) & "\n"
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
  of ogNone: discard

func `$`*(org: OrgNode): string = serialize(org)

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

proc stripFirstSpace(org: OrgNode): OrgNode =
  doAssert org.kind == ogText
  result = org
  result.text = result.text.strip(leading = true)

template addMaybeStrip(s, tok, tokens, first: untyped): untyped =
  ## Parses the token, strips potential whitespace at the beginning (if `first`) and
  ## strip newlines at the very, if the token was a newline.
  ##
  ## Used for the `title` of a Section and the value of a Property.
  var node = parseToken(tok, tokens)
  if first:
    node = node.stripFirstSpace()
    first = false
  if tok.kind != tkNewline:
    s.add node

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
    var val = newOrgArray()
    var first = true # to strip the first whitespace
    while tokens.len > 0 and tok.kind != tkNewline:
      tok = tokens.pop()
      val.addMaybeStrip(tok, tokens, first)
    result = OrgNode(kind: ogProperty, prop: Property(key: key, value: val))
  else: # neither, just a colon. Is included in `key`, so drop `t`
    result = OrgNode(kind: ogText, text: key)

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

proc skipNewlines(tokens: seq[Token]): int =
  result = tokens.high
  while result > 0 and tokens[result].kind == tkNewline:
    dec result

proc printTokens(t: seq[Token], num: int) =
  var idx = t.high
  for i in 0 ..< num:
    if idx >= 0:
      echo "Token: ", t[idx]
    dec idx

proc determineSectionLevel(tokens: seq[Token], t = Token(kind: tkNone)): int =
  ## Determines the level of the next section. It _must_ be a section.
  ## An optional newline as the next token is allowed.
  if t.kind in {tkNewline, tkNone}:
    result = 0
  elif t.kind == tkStar:
    result = 1
  else:
    raiseAssert "Invalid argument to `determineSectionLevel`, not a section: " & $t
  var i = skipNewlines(tokens)
  while i > 0 and tokens[i].kind == tkStar:
    inc result
    dec i

proc parseTitle(t: Token, tokens: var seq[Token]): (OrgNode, seq[string]) =
  ## Parses a title from a section. Must only be called if `t` is not an
  ## ident and the previous token is a `tkStar`.
  doAssert t.kind != tkIdent, "Ident cannot be valid title" & $t
  if t.kind == tkNewline: return (newOrgEmpty(), @[])
  else:
    result[0] = newOrgArray()
    var tok: Token
    var first = true # to strip the first space after `*`
    var tags: seq[string]
    while tokens.len > 0 and tok.kind != tkNewline: # every token to next newline is part of the title
      tok = tokens.pop()
      case tok.kind
      of tkStar: continue # skip, already taken into account by `determineSectionLevel`
      of tkColon: # maybe found a tag!
        case tokens.tryPeek().kind
        of tkIdent: # probably a tag
          let tag = tokens.pop() # get the likely tag
          if tokens.tryPeek().kind != tkColon: # must end with colon, not a tag after all
            var nodeTok = tok.parseToken(tokens)
            var nodeTag = tag.parseToken(tokens)
            result[0].add nodeTok # add to title itself
            result[0].add nodeTag
          else: # is indeed a tag
            tags.add $tag.parseToken(tokens)
        of tkNewline: # was the last tag, ignore newline
          continue
        else:
          raiseAssert "Invalid token for title: " & $t & " at line: " & $t.line & ", col: " & $t.column
      else:
        var node = tok.parseToken(tokens)
        result[0].addMaybeStrip(tok, tokens, first)
    result[1] = tags

proc parseOperatorOrSection(t: Token, tokens: var seq[Token]): OrgNode =
  let level = determineSectionLevel(tokens, t)
  let tn = tokens.tryPeek() # only peek!
  if tn.kind == tkIdent: # should be an operator
    result = tryParseOperator(t, tokens, tkStar, ogBold)
  else: # should be a section
    var body = newOrgArray()
    let (title, tags) = parseTitle(tn, tokens)
    while tokens.len > 0:
      if sectionUpcoming(tokens):
        let nextLevel = determineSectionLevel(tokens)
        if nextLevel <= level:
          break # exit if we've reached a section of the same or higher level
      let tn = tokens.pop()
      body.add tn.parseToken(tokens)
    result = OrgNode(kind: ogSection, sec: Section(title: title, tags: tags, level: level, body: body))

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

proc parse*(tokens: seq[Token]): OrgNode =
  ## `Tokens` is in reverse order. That way we can just `pop`.
  var tokens = tokens.reversed()
  result = newOrgArray()
  while tokens.len > 0:
    let t = tokens.pop()
    ## XXX: concat two text nodes if they appear after another!
    result.add t.parseToken(tokens)

proc parseOrg*(fname: string): OrgNode =
  ## Parser an Org file from a given filename.
  result = readFile(fname).tokenize.parse

iterator body*(org: OrgNode): OrgNode =
  ## Iterates over the body of the org section
  if org.kind != ogSection:
    raise newException(ValueError, "The argument is not a section, but a " & $org.kind)
  for el in org.sec.body:
    yield el

iterator subsections*(org: OrgNode): OrgNode =
  ## Yields all subsections in the given org section `org`.
  doAssert org.kind == ogSection, "Input node is not an Org mode section, but: " & $org.kind
  for ch in body(org):
    case ch.kind
    of ogSection: yield ch
    else: continue

proc getTitle*(org: OrgNode): OrgNode =
  ## Returns the title of the given section
  doAssert org.kind == ogSection, "Input node is not an Org mode section, but: " & $org.kind
  result = org.sec.title

proc getBody*(org: OrgNode): OrgNode =
  ## Returns the body of the given section without any property list
  doAssert org.kind == ogSection, "Input node is not an Org mode section, but: " & $org.kind
  result = newOrgArray()
  for el in body(org):
    if el.kind == ogPropertyList: continue
    result.add el

proc getBodyText*(org: OrgNode): OrgNode =
  ## Returns the body of the given section without any property list *or* any subsections.
  doAssert org.kind == ogSection, "Input node is not an Org mode section, but: " & $org.kind
  result = newOrgArray()
  for el in body(org):
    if el.kind == ogPropertyList: continue
    if el.kind == ogSection: break # done. Cannot be anything after first subsection
    result.add el


proc propertyList*(org: OrgNode): PropertyList =
  ## Returns the property list of the given section, if any.
  if org.kind != ogSection: return
  for el in body(org):
    if el.kind == ogPropertyList:
      result = el.propList

proc getProperty*(x: PropertyList, key: string): Option[Property] =
  ## Returns the propery with the given `key`.
  for el in x:
    if el.key == key: return some(el)

proc findSection*(org: OrgNode, sec: string): OrgNode =
  ## Returns the section with name `sec` or `:CUSTOM_ID: sec`.
  ## A custom ID always matches at higher priority. If you have multiple
  ## ids / titles with the same name, we return the first (that is not
  ## intended usage).
  proc findSecImpl(x: OrgNode): OrgNode =
    ## Returns either a `ogSection` matching `sec` or returns an empty
    ## node.
    doAssert x.kind == ogSection
    let pList = propertyList(x)
    let pCustom = getProperty(pList, "CUSTOM_ID")
    if pCustom.isSome and $pCustom.unsafeGet.value == sec: return x
    elif $x.sec.title == sec: return x
    else: # check subsections
      for el in subsections(x):
        let res = findSecImpl(el)
        if res.kind == ogSection: return res
    result = newOrgEmpty()

  for el in org:
    if el.kind == ogSection:
      let s = findSecImpl(el)
      if s.kind == ogSection: return s

proc hasTag*(org: OrgNode, tag: string): bool =
  ## Returns `true` if the given Org section has the given `tag`.
  doAssert org.kind == ogSection, "Input node is not an Org mode section, but: " & $org.kind
  result = tag in org.sec.tags

when isMainModule:
  const path = "/home/basti/org/Documents/CV_data.org"
  let data = readFile(path)

  var toks = tokenize(data)
  echo "Tokens: ", toks

  let org = parse(toks)

  echo org
