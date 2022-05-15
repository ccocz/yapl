# Makefile for building the lexer, parser and interpreter.

GHC        = ghc
SRC		   = src
BUILD	   = build
PARSER_DIR = src/parser
GHC_OPTS   = --make -Wall -outputdir ${BUILD} -i${PARSER_DIR}:${SRC}
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc
BNFC	   = bnfc
BNFC_OPTS  = --haskell --functor -o ${PARSER_DIR}
GRAMMAR	   = grammar

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : interpreter

# Rules for building the parser.

${PARSER_DIR}/AbsYAPL.hs ${PARSER_DIR}/LayoutYAPL.hs ${PARSER_DIR}/LexYAPL.x ${PARSER_DIR}/ParYAPL.y ${PARSER_DIR}/PrintYAPL.hs ${PARSER_DIR}/TestYAPL.hs : ${GRAMMAR}/YAPL.cf
	export PATH="${PATH}:/home/students/inf/PUBLIC/MRJP/bin" ;
	${BNFC} ${BNFC_OPTS} ${GRAMMAR}/YAPL.cf

${PARSER_DIR}/%.hs : ${PARSER_DIR}/%.y
	${HAPPY} ${HAPPY_OPTS} $<

${PARSER_DIR}/%.hs : ${PARSER_DIR}/%.x
	${ALEX} ${ALEX_OPTS} $<

PARSER = ${PARSER_DIR}/AbsYAPL.hs ${PARSER_DIR}/LayoutYAPL.hs ${PARSER_DIR}/LexYAPL.hs ${PARSER_DIR}/ParYAPL.hs ${PARSER_DIR}/PrintYAPL.hs ${PARSER_DIR}/TestYAPL.hs
IMP = ${SRC}/EnvYAPL.hs ${SRC}/ExecYAPL.hs

TestYAPL : ${PARSER}
	${GHC} ${GHC_OPTS} $@

interpreter : ${PARSER} ${IMP}
	${GHC} ${GHC_OPTS} ${SRC}/interpreter.hs -o $@

# Rules for cleaning generated files.

clean:
	-rm -rf ${PARSER_DIR} ${BUILD} interpreter

# EOF
