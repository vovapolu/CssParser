package cssparser

// According to https://www.w3.org/TR/css-syntax-3/

object CssParser {

  import fastparse.all._
  import fastparse.parsers.Terminals.AnyChar

  val comment = P( "/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")

  val newline = P( "\n" | "\r\n" | "\r" | "\f")

  val whitespace = P( " " | "\t" | newline)

  val hex_digit = P( CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  val escape = P( "\\" ~ ((!(newline | hex_digit) ~ AnyChar) |
    (hex_digit.rep(min=1, max=6) ~ whitespace.?)) )

  val whitespace_token = P( whitespace.rep(1) ).log()

  val ws = P( whitespace_token.rep )

  val ident_token = P( ("-".? ~ (CharIn('a' to 'z', 'A' to 'Z', "_") | escape) ~
    (CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_-") | escape).rep).! ).log() map Ast.IdentToken

  val function_token = P( ident_token.! ~ "(" ).log() map Ast.FunctionToken

  val at_word_token = P( "@" ~ ident_token.! ) map Ast.AtWordToken

  val hash_token = P( "#" ~
    (CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_-") | escape).rep(1).! ) map Ast.HashWordToken

  val string_token_char = ((!("\"" | "'" | "\\" | newline ) ~ AnyChar) | escape | ("\\" ~ newline)).log()

  val string_token = P( ("\"" ~/ string_token_char.rep.! ~/ "\"") |
    ("'" ~/ string_token_char.rep.! ~/ "'") ).log() map Ast.StringToken

  val url_unquoted = P( ((!(CharIn("\"\'()\\") | whitespace) ~ AnyChar) | escape).rep(1) )

  val url_token = P( "url(" ~/ (ws ~ (url_unquoted.! | string_token.!) ~ ws).?.! ~/ ")" ) map Ast.UrlToken

  val digit = P( CharIn('0' to '9') )

  val number_token = P( ((CharIn("+-").? ~ (digit.rep(1) ~ "." ~ digit.rep(1))) | digit.rep(1) | ("." ~ digit.rep(1) ~
    (CharIn("eE") ~ CharIn("+-").? ~ digit.rep(1)).?)).! ) map Ast.NumberToken

  val dimension_token = P( number_token.! ~ ident_token.! ) map
    { case (number, ident) => Ast.DimensionToken(number, ident)}

  val percentage_token = P( number_token.! ~ "%" ).log() map Ast.PercentageToken

  val unicode_range_token = P( CharIn("Uu") ~ "+" ~ hex_digit.rep(min=1, max=6).! |
    (hex_digit.rep(min=1, max=5).! flatMap (s => "?".rep(min=1, max=6 - s.length))).! |
    (hex_digit.rep(min=1, max=6).! ~ "-" ~ hex_digit.rep(min=1, max=6).!) ) map {
      case hex: String => Ast.UnicodeRangeToken(hex, hex)
      case (left: String, right: String) => Ast.UnicodeRangeToken(left, right)
    }

  val include_match_token = P( "~=" ) map {_ => Ast.IncludeMatchToken}
  val dash_match_token = P( "|=" ) map {_ => Ast.DashMatchToken}
  val prefix_match_token = P( "^=" ) map {_ => Ast.PrefixMatchToken}
  val suffix_match_token = P( "$=" ) map {_ => Ast.SuffixMatchToken}
  val substring_match_token = P( "*=" ) map {_ => Ast.SubstringMatchToken}
  val column_token = P( "||" ) map {_ => Ast.ColumnToken}
  val CDO_token = P( "<!--" ) map {_ => Ast.CdoToken}
  val CDC_token = P( "-->" ) map {_ => Ast.CdcToken}

  val delim_token = P( CharIn("#$*+,-./:;<>@^~=").! ).log() map Ast.DelimToken

  // any token except function_token
  val simple_token: Parser[Option[Ast.SimpleToken]] = P(
    whitespace_token | percentage_token |
    dimension_token | unicode_range_token |
    url_token  | at_word_token | hash_token |
    string_token | ident_token | number_token |
    include_match_token | dash_match_token |
    prefix_match_token | suffix_match_token |
    substring_match_token | column_token |
    CDO_token | CDC_token | delim_token ).log() map {
      case st: Ast.SimpleToken => Some(st)
      case _ => None
    }

  val bracketsBlock = P( "(" ~ componentValue.rep ~ ")" ) map (values => Ast.BracketsBlock(values.flatten))
  val curlyBracketsBlock = P( "{" ~ componentValue.rep ~ "}" ) map (values => Ast.CurlyBracketsBlock(values.flatten))
  val squareBracketsBlock = P( "[" ~ componentValue.rep ~ "]" ) map (values => Ast.SquareBracketsBlock(values.flatten))

  val functionBlock = P( function_token ~ componentValue.rep ~ ")").log() map {
    case (Ast.FunctionToken(name), values: Seq[Option[Ast.ComponentValue]]) => Ast.FunctionBlock(name, values.flatten)
  }

  val componentValue: Parser[Option[Ast.ComponentValue]] = P( simple_token | bracketsBlock |
    curlyBracketsBlock | squareBracketsBlock | functionBlock ).log() map {
      case cv: Ast.ComponentValue => Some(cv)
      case Some(cv: Ast.ComponentValue) => Some(cv)
      case _ => None
    }

  val important = P( "!" ~ ws ~ "important" ~ ws).log()

  val declaration = P( ident_token.! ~ ws ~ ":" ~ componentValue.rep ~ important.!.?).log() map {
    case (ident, values, Some(_)) => Ast.Declaration(ident, values.flatten, isImportant = true)
    case (ident, values, None) => Ast.Declaration(ident, values.flatten, isImportant = false)
  }

  val atRule = P( at_word_token ~ (!CharIn(";{") ~ componentValue).rep ~ (";" | curlyBracketsBlock) ).log() map {
    case (Ast.AtWordToken(name), values, block: Ast.CurlyBracketsBlock) => Ast.AtRule(name, values.flatten, Some(block))
    case (Ast.AtWordToken(name), values, _) => Ast.AtRule(name, values.flatten, None)
  }

  val qualifiedRule = P( (!"{" ~ componentValue).rep ~ curlyBracketsBlock ).log() map {
    case (values, block) => Ast.QualifiedRule(values.flatten, block)
  }

  val declarationList = P( ws ~ (";" | atRule | declaration).rep ).log() map (
    s => Ast.DeclarationList(s flatMap {
      case atRule: Ast.AtRule => Some(Right(atRule))
      case declaration: Ast.Declaration => Some(Left(declaration))
      case _ => None
    }))

  val ruleList = P( (whitespace_token | qualifiedRule | atRule).rep ).log() map (
    s => Ast.RuleList(s flatMap {
      case rule: Ast.Rule => Some(rule)
      case _ => None
    }))

  val stylesheet = P( (CDO_token | CDC_token | whitespace_token | qualifiedRule | atRule).rep ).log() map (
    s => Ast.Stylesheet(s flatMap {
      case rule: Ast.Rule => Some(Left(rule))
      case ctoken: Ast.CToken => Some(Right(ctoken))
      case _ => None
    }))
}
