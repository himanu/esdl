// Written in the D programming language.

// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>


// This file is a part of esdl.
// This file is a part of VLang.

// This file contains functionality to translate constraint block into
// functions that are used for solving the constraints. Since this
// translator is invoked by vlang at compile time, the code is
// optimized for memory. The translator works in two phases. In the
// first phase we just parse through the code to get an idea of the
// length of the resulting string. In the second phase we actually
// translate the constraint block

// The logic of this translator is simple. Here is what we need to
// achieve:
// 1. If we hit a foreach or if/else block, we need to handle it: TBD
// 2. We need to replace all the identifiers and literals with a
// wrapper around them
// 3. All the compariason operators and the implication operator need
// to be replaced with corresponding function calls. The good thing is
// that none of none of these operators chain together

// Here is how we do it:
// 
// 1. Treat all parenthesis, addition, multiplication, substraction
// other lower level operators as whitespace. These just need to be
// replicated in the output
// 2. start from left, reserve space for two left parenthesis, one for
// possible implication operator and another for possible compariason
// operator. Keep two local size_t variables to track these paren
// positions.
// 3. Any logic operator "&&" or "||" or an implication operator "=>"
// or a semicolon will will terminate a comparion. We also keep track
// of whether we are inside a comparison operator using a bool
// variale.
// 4. a semicolon alone can end an implication operator.

// As for foreach block, we need to keep a mapping table to help
// replace the identifiers as required. We shall use two dynamic
// arrays to help achieve that. TBD

module esdl.data.cstx;

enum Token: byte
  {   NONE = 0,
      ADD,
      SUB,
      MUL,
      DIV,
      LSH,
      RSH,
      EQU,
      GTE,
      LTE,
      NEQ,
      GTH,
      LTH,
      IMP,			// Implication operator
      AND,
      OR,
      END,			// End of statement, semicolon
      // VAR,			// An identifier or a literal
      // IF,			// A keyword, if, else, or foreach
      // ELSE,
      // FOREACH,
      // RPL,			// A mapped variable
      }
      
    

enum Tokens: string
  {   ADD = "+",
      SUB = "-",
      MUL = "*",
      DIV = "/",
      LSH = "<<",
      RSH = ">>",
      EQU = "==",
      GTE = ">=",
      LTE = "<=",
      NEQ = "!=",
      GTH = ">",
      LTH = "<",
      IMP = "=>", 		// Implication operator
      AND = "&&",
      OR  = "||",
      LCMTS = "//",		// Line comment start
      BCMTS = "/*",		// block comment start
      NCMTS = "/+", 		// nested comment start
      LCMTE = "/n",		// line comment end
      BCMTE = "*/",		// block comment end
      NCMTE = "+/",		// nested comment end
      }

Token parseOperator(string CST, ref size_t cursor) {
  Token tok = Token.NONE;
  if(cursor < CST.length - 1) {
    if(CST[cursor] == '<' && CST[cursor+1] == '<') tok = Token.LSH;
    if(CST[cursor] == '>' && CST[cursor+1] == '>') tok = Token.RSH;
    if(CST[cursor] == '=' && CST[cursor+1] == '=') tok = Token.EQU;
    if(CST[cursor] == '>' && CST[cursor+1] == '=') tok = Token.GTE;
    if(CST[cursor] == '<' && CST[cursor+1] == '=') tok = Token.LTE;
    if(CST[cursor] == '!' && CST[cursor+1] == '=') tok = Token.NEQ;
    if(CST[cursor] == '&' && CST[cursor+1] == '&') tok = Token.AND;
    if(CST[cursor] == '|' && CST[cursor+1] == '|') tok = Token.OR;
    if(CST[cursor] == '=' && CST[cursor+1] == '>') tok = Token.IMP;
  }
  if(tok !is Token.NONE) {
    cursor += 2;
    return tok;
  }
  if(cursor < CST.length) {
    if(CST[cursor] == '+') tok = Token.ADD;
    if(CST[cursor] == '-') tok = Token.SUB;
    if(CST[cursor] == '*') tok = Token.MUL;
    if(CST[cursor] == '/') tok = Token.DIV;
    if(CST[cursor] == '<') tok = Token.LTH;
    if(CST[cursor] == '>') tok = Token.GTH;
    if(CST[cursor] == ';') tok = Token.END;
  }
  if(tok !is Token.NONE) {
    cursor += 1;
    return tok;
  }
  return tok;			// None
}

void errorToken(string CST, ref size_t cursor) {
  size_t start = cursor;
  while(cursor < CST.length) {
    char c = CST[cursor];
    if(c !is ' ' && c !is '\n' && c !is '\t' && c !is '\r' && c !is '\f') {
      ++cursor;
    }
    else break;
  }
  if(cursor == start) {
    assert(false, "EOF while parsing!");
  }
  assert(false, "Unrecognized token: " ~ "'" ~ CST[start..cursor] ~ "'");
}

size_t parseIdentifier(string CST, ref size_t cursor) {
  size_t start = cursor;
  if(cursor < CST.length) {
    char c = CST[cursor];
    if((c >= 'A' && c <= 'Z') ||
       (c >= 'a' && c <= 'z') ||
       (c == '_')) {
      ++cursor;
    }
    else {
      return 0;
    }
  }
  while(cursor < CST.length) {
    char c = CST[cursor];
    if((c >= 'A' && c <= 'Z') ||
       (c >= 'a' && c <= 'z') ||
       (c >= '0' && c <= '9') ||
       (c == '_' || c == '.')) {
      ++cursor;
    }
    else {
      break;
    }
  }
  return cursor - start;
}

size_t parseLineComment(in string CST, ref size_t cursor) {
  size_t start = cursor;
  if(cursor >= CST.length - 2 ||
     CST[cursor] != '/' || CST[cursor+1] != '/') return 0;
  else {
    cursor += 2;
    while(cursor < CST.length) {
      if(CST[cursor] == '\n') {
	break;
      }
      else {
	if(cursor == CST.length) {
	  // commment unterminated
	  assert(false, "Line comment not terminated");
	}
      }
      cursor += 1;
    }
    cursor += 1;
    return cursor - start;
  }
}

unittest {
  size_t curs = 4;
  assert(parseLineComment("Foo // Bar;\n\n", curs) == 8);
  assert(curs == 12);
}

size_t parseBlockComment(in string CST, ref size_t cursor) {
  size_t start = cursor;
  if(cursor >= CST.length - 2 ||
     CST[cursor] != '/' || CST[cursor+1] != '*') return 0;
  else {
    cursor += 2;
    while(cursor < CST.length - 1) {
      if(CST[cursor] == '*' && CST[cursor+1] == '/') {
	break;
      }
      else {
	if(cursor == CST.length - 1) {
	  // commment unterminated
	  assert(false, "Block comment not terminated");
	}
      }
      cursor += 1;
    }
    cursor += 2;
    return cursor - start;
  }
}

unittest {
  size_t curs = 4;
  assert(parseBlockComment("Foo /* Bar;\n\n */", curs) == 12);
  assert(curs == 16);
}

size_t parseNestedComment(in string CST, ref size_t cursor) {
  size_t nesting = 0;
  size_t start = cursor;
  if(cursor >= CST.length - 2 ||
     CST[cursor] != '/' || CST[cursor+1] != '+') return 0;
  else {
    cursor += 2;
    while(cursor < CST.length - 1) {
      if(CST[cursor] == '/' && CST[cursor+1] == '+') {
	nesting += 1;
	cursor += 1;
      }
      else if(CST[cursor] == '+' && CST[cursor+1] == '/') {
	if(nesting == 0) {
	  break;
	}
	else {
	  nesting -= 1;
	  cursor += 1;
	}
      }
      cursor += 1;
      if(cursor >= CST.length - 1) {
	// commment unterminated
	assert(false, "Block comment not terminated");
      }
    }
    cursor += 2;
    return cursor - start;
  }
}

unittest {
  size_t curs = 4;
  parseNestedComment("Foo /+ Bar;/+// \n+/+*/ +/", curs);
  assert(curs == 25);
}

size_t parseLiteral(in string CST, ref size_t cursor) {
  size_t start = cursor;
  while(cursor < CST.length) {
    char c = CST[cursor];
    if((c >= '0' && c <= '9') ||
       (c == '_')) {
      ++cursor;
    }
    else {
      break;
    }
  }
  // Look for long/short specifier
  while(cursor < CST.length) {
    char c = CST[cursor];
    if(c == 'L' || c == 'S' ||  c == 'U') {
      ++cursor;
    }
    else {
      break;
    }
  }
  return cursor - start;
}

unittest {
  size_t curs = 4;
  assert(parseIdentifier("Foo Bar;", curs) == 3);
  assert(curs == 7);
}


size_t parseWhiteSpace(in string CST, ref size_t cursor) {
  auto start = cursor;
  while(cursor < CST.length) {
    auto c = CST[cursor];
    // eat up whitespaces
    if(c is ' ' || c is '\n' || c is '\t' || c is '\r' || c is '\f') {
      ++cursor;
      continue;
    }
    else {
      break;
    }
  }
  return cursor - start;
}

size_t parseLeftSpace(in string CST, ref size_t cursor) {
  auto start = cursor;
  while(cursor < CST.length) {
    if(parseWhiteSpace(CST, cursor) ||
       parseLineComment(CST, cursor) ||
       parseBlockComment(CST, cursor) ||
       parseNestedComment(CST, cursor)) {
      continue;
    }
    else {
      if(cursor < CST.length && CST[cursor] == '(') {
	++cursor;
	continue;
      }
      else {
	break;
      }
    }
  }
  return cursor - start;
}

size_t parseRightSpace(in string CST, ref size_t cursor) {
  auto start = cursor;
  while(cursor < CST.length) {
    if(parseWhiteSpace(CST, cursor) ||
       parseLineComment(CST, cursor) ||
       parseBlockComment(CST, cursor) ||
       parseNestedComment(CST, cursor)) {
      continue;
    }
    else {
      if(cursor < CST.length && CST[cursor] == ')') {
	++cursor;
	continue;
      }
      else {
	break;
      }
    }
  }
  return cursor - start;
}

size_t parseSpace(in string CST, ref size_t cursor) {
  auto start = cursor;
  while(cursor < CST.length) {
    if(parseWhiteSpace(CST, cursor) ||
       parseLineComment(CST, cursor) ||
       parseBlockComment(CST, cursor) ||
       parseNestedComment(CST, cursor)) {
      continue;
    }
    else {
      break;
    }
  }
  return cursor - start;
}

unittest {
  size_t curs = 0;
  assert(parseLeftSpace("    // foo\nFoo Bar;", curs) == 11);
  assert(curs == 11);
}

void calculateSize(in string CST, out size_t size1, out size_t size2) {
  // size_t cursor = 0;
  // size_t countVar = 0;
  // size_t countCmp = 0;
  // size_t countLOp = 0;
  // size_t countExp = 0;
  // parseLeftSpace(CST, cursor);
  // while(cursor < CST.length) {
  //   countExp += 1;
  //   if(parseIdentifier(CST, cursor) == 0 &&
  //      parseLiteral(CST, cursor) == 0) {
  //     errorToken(CST, cursor);
  //   }
  //   else countVar += 1;
  //   parseRightSpace(CST, cursor);
  //   Token opToken = parseOperator(CST, cursor);
  //   if(opToken is Token.NONE) {
  //     errorToken(CST, cursor);
  //   }
  //   else if(opToken == Token.EQU || opToken == Token.NEQ ||
  // 	    opToken == Token.LTE || opToken == Token.GTE ||
  // 	    opToken == Token.LTH || opToken == Token.GTH) {
  //     countCmp += 1;
  //   }
  //   else if(opToken == Token.IMP || opToken == Token.AND ||
  // 	    opToken == Token.OR  || opToken == Token.END) {
  //     countLOp += 1;
  //   }
  //   if(cursor != CST.length
  //      // && varMap.length != 0
  //      ) {
  //     if(CST[cursor] == '}') {
  // 	cursor++;
  // 	break;
  //     }
  //   }
  //   parseLeftSpace(CST, cursor);
  // }
  // debug(CSTSPACE) {
  //   import std.stdio;
  //   writeln("countVar:", countVar, " countExp:", countExp,
  // 	    " countCmp:", countCmp, " countLOp:", countLOp,
  // 	    " length:", CST.length);
  // }
  // size1 = countVar*32 + CST.length + 64;
  // size2 = countVar*28 + countExp*4 + countCmp*8 + countLOp*8 + CST.length + 128;
  size1 = 1000;
  size2 = 1000;
}

unittest {
  // assert(calculateSize("FOO;") == 100);
  // assert(calculateSize("FOO > BAR;") == 146);
}

size_t fill(in string source, char[] target, size_t cursor = 0) {
  foreach(i, c; source) {
    target[cursor+i] = c;
  }
  return cursor + source.length;
}

char[] translate(in string CST) {
  char[] buffer;

  size_t size1, size2;
  size_t srcCursor = 0;
  size_t dstCursor = 0;


  calculateSize(CST, size1, size2);

  buffer.length = size1 + 2 * size2;

  foreach(ref char c; buffer) {
    c = ' ';
  }

  dstCursor = fill("\noverride public CstBlock getCstExpr() {"
		   "\n  auto cstExpr = new CstBlock;\n",
		   buffer, dstCursor);

  translateBlock(CST, buffer, srcCursor, dstCursor);

  return buffer;
}

int idMatch(string id, xVar[] varMap) {
  foreach(int i, var; varMap) {
    if(var.varName == id) return i;
  }
  return -1;
}

// Variable translation map
struct xVar {
  string varName;
  string xLat;
}

void translateForeach(in string CST, char[] buffer, ref size_t srcCursor,
		      ref size_t dstCursor, xVar[] varMap = []) {
  string index;
  string elem;
  string array;
  size_t srcTag;
  
  parseSpace(CST, srcCursor);
  if(CST[srcCursor] != '(') {
    errorToken(CST, srcCursor);
  }
  ++srcCursor;
  parseSpace(CST, srcCursor);

  srcTag = srcCursor;

  // Parse the index
  if(parseIdentifier(CST, srcCursor) > 0) {
    // FIXME -- check if the variable names do not shadow earlier
    // names in the table
    index = CST[srcTag..srcCursor];
    parseSpace(CST, srcCursor);
    if(CST[srcCursor] == ';') {
      elem = index;
      index = "";
    }
    else if(CST[srcCursor] != ',') {
      errorToken(CST, srcCursor);
    }
    ++srcCursor;
    parseSpace(CST, srcCursor);
  }
  else {
    errorToken(CST, srcCursor);
  }
  
  // Parse elem
  if(elem.length is 0) {
    srcTag = srcCursor;
    if(parseIdentifier(CST, srcCursor) > 0) {
      // FIXME -- check if the variable names do not shadow earlier
      // names in the table
      elem = CST[srcTag..srcCursor];
      parseSpace(CST, srcCursor);
      if(CST[srcCursor] != ';') {
	errorToken(CST, srcCursor);
      }
      ++srcCursor;
      parseSpace(CST, srcCursor);
    }
    else {
      errorToken(CST, srcCursor);
    }
  }

  // Parse array
  srcTag = srcCursor;
  if(parseIdentifier(CST, srcCursor) > 0) {
    // FIXME -- check if the variable names do not shadow earlier
    // names in the table
    array = CST[srcTag..srcCursor];
    parseSpace(CST, srcCursor);
    if(CST[srcCursor] != ')') {
      errorToken(CST, srcCursor);
    }
    ++srcCursor;
    parseSpace(CST, srcCursor);
  }
  else {
    errorToken(CST, srcCursor);
  }

  if(CST[srcCursor] != '{') {
    errorToken(CST, srcCursor);
  }
  ++srcCursor;

  // add index
  if(index.length != 0) {
    xVar x;
    x.varName = index;
    x.xLat = "_esdl__cstRandArrIndex!q{" ~ array ~ "}(_outer)";
    varMap ~= x;
  }
    
  xVar x;
  x.varName = elem;
  x.xLat = "_esdl__cstRandArrElem!q{" ~ array ~ "}(_outer)";
  varMap ~= x;

  translateBlock(CST, buffer, srcCursor, dstCursor, varMap);

  if(index.length != 0) {
    varMap = varMap[0..$-2];
  }
  else {
    varMap = varMap[0..$-1];
  }
  
}

void translateBlock(in string CST, char[] buffer, ref size_t srcCursor,
		    ref size_t dstCursor, xVar[] varMap = []) {
  bool cmpRHS;
  bool andRHS;
  bool orRHS;
  bool impRHS;

  size_t srcTag = 0;

  size_t cmpDstAnchor = 0;
  size_t andDstAnchor = 0;
  size_t orDstAnchor  = 0;
  size_t impDstAnchor = 0;

  bool newStatement = true;

  while(srcCursor < CST.length) {
    srcTag = srcCursor;
    parseSpace(CST, srcCursor);
    dstCursor = fill(CST[srcTag..srcCursor],
		     buffer, dstCursor);

    if(srcCursor == CST.length) break;

    // countExp += 1;

    srcTag = srcCursor;
    parseLeftSpace(CST, srcCursor);

    dstCursor = fill(CST[srcTag..srcCursor],
		     buffer, dstCursor);

    srcTag = srcCursor;
    if(parseIdentifier(CST, srcCursor) > 0) {
      if(CST[srcTag..srcCursor] == "foreach") {
	translateForeach(CST, buffer, srcCursor,
			 dstCursor, varMap);
	continue;
      }

      if(newStatement is true) {
	dstCursor = fill("  cstExpr ~= ", buffer, dstCursor);
	cmpDstAnchor = dstCursor++;
	andDstAnchor = dstCursor++;
	orDstAnchor  = dstCursor++;
	impDstAnchor = dstCursor++;
	newStatement = false;
      }

      int idx = idMatch(CST[srcTag..srcCursor], varMap);
      if(idx == -1) {
	dstCursor = fill("_esdl__cstRand!q{", buffer, dstCursor);
	dstCursor = fill(CST[srcTag..srcCursor],
			 buffer, dstCursor);
	dstCursor = fill("}(_outer)", buffer, dstCursor);
      }
      else {
      	dstCursor = fill(varMap[idx].xLat, buffer, dstCursor);
      }
    }
    else if(parseLiteral(CST, srcCursor) > 0) {
      dstCursor = fill("_esdl__cstRand(", buffer, dstCursor);
      dstCursor = fill(CST[srcTag..srcCursor],
		       buffer, dstCursor);
      dstCursor = fill(", _outer)", buffer, dstCursor);
    }
    else {
      errorToken(CST, srcCursor);
    }

    srcTag = srcCursor;
    parseRightSpace(CST, srcCursor);
    dstCursor = fill(CST[srcTag..srcCursor],
		     buffer, dstCursor);

    srcTag = srcCursor;
    Token opToken = parseOperator(CST, srcCursor);

    final switch(opToken) {
    case Token.NONE:
      errorToken(CST, srcCursor);
      break;
    case Token.END:
      if(cmpRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	cmpRHS = false;
      }
      if(andRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	andRHS = false;
      }
      if(orRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	orRHS = false;
      }
      if(impRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	impRHS = false;
      }
      dstCursor = fill(";\n", buffer, dstCursor);
      newStatement = true;
      break;
    case Token.IMP:
      if(cmpRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	cmpRHS = false;
      }
      if(andRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	andRHS = false;
      }
      if(orRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	orRHS = false;
      }
      fill("(", buffer, impDstAnchor);
      dstCursor = fill(").imp (", buffer, dstCursor);
      cmpDstAnchor = dstCursor++;
      andDstAnchor = dstCursor++;
      orDstAnchor  = dstCursor++;
      impRHS = true;
      break;
    case Token.OR:		// take care of cmp/and
      if(cmpRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	cmpRHS = false;
      }
      if(andRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	andRHS = false;
      }
      if(orRHS !is true) {
	fill("(", buffer, orDstAnchor);
	orRHS = true;
      }
      dstCursor = fill(") | (", buffer, dstCursor);
      cmpDstAnchor = dstCursor++;
      andDstAnchor = dstCursor++;
      break;
    case Token.AND:		// take care of cmp
      if(cmpRHS is true) {
	dstCursor = fill(")", buffer, dstCursor);
	cmpRHS = false;
      }
      if(andRHS !is true) {
	fill("(", buffer, andDstAnchor);
	andRHS = true;
      }
      dstCursor = fill(") & (", buffer, dstCursor);
      cmpDstAnchor = dstCursor++;
      break;
    case Token.EQU:
      fill("(", buffer, cmpDstAnchor);
      cmpRHS = true;
      dstCursor = fill(").equ (", buffer, dstCursor);
      break;
    case Token.NEQ:
      fill("(", buffer, cmpDstAnchor);
      cmpRHS = true;
      dstCursor = fill(").neq (", buffer, dstCursor);
      break;
    case Token.LTE:
      fill("(", buffer, cmpDstAnchor);
      cmpRHS = true;
      dstCursor = fill(").lte (", buffer, dstCursor);
      break;
    case Token.GTE:
      fill("(", buffer, cmpDstAnchor);
      cmpRHS = true;
      dstCursor = fill(").gte (", buffer, dstCursor);
      break;
    case Token.LTH:
      fill("(", buffer, cmpDstAnchor);
      cmpRHS = true;
      dstCursor = fill(").lth (", buffer, dstCursor);
      break;
    case Token.GTH:
      fill("(", buffer, cmpDstAnchor);
      cmpRHS = true;
      dstCursor = fill(").gth (", buffer, dstCursor);
      break;
    case Token.ADD:
      dstCursor = fill("+", buffer, dstCursor);
      break;
    case Token.SUB:
      dstCursor = fill("-", buffer, dstCursor);
      break;
    case Token.MUL:
      dstCursor = fill("*", buffer, dstCursor);
      break;
    case Token.DIV:
      dstCursor = fill("/", buffer, dstCursor);
      break;
    case Token.LSH:
      dstCursor = fill("<<", buffer, dstCursor);
      break;
    case Token.RSH:
      dstCursor = fill(">>", buffer, dstCursor);
      break;
    }

    srcTag = srcCursor;
    parseSpace(CST, srcCursor);

    dstCursor = fill(CST[srcTag..srcCursor],
		     buffer, dstCursor);

    // if not the main block, look for curly braces to end the block
    if(srcCursor != CST.length
       && varMap.length != 0
       ) {
      if(CST[srcCursor] == '}') {
	srcCursor++;
	break;
      }
    }

  }

  fill("\n  return cstExpr;\n}\n", buffer, dstCursor);
}


unittest {
  // assert(translate("FOO;"));
  // assert(translate("FOO > BAR;"));
  // assert(translate("FOO > BAR || FOO == BAR;"));
  //                012345678901234567890123456789012345678901234567890123456789
  assert(translate("_num_seq <= 2 || seq_kind1 >= 2 ;  seq_kind2 <  _num_seq || seq_kind3 == 0;
                   "));
}
