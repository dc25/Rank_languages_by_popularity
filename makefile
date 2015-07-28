HASKELL_SOURCES = Main.hs RosettaApi.hs

OUTPUT_DIR=dist/build/Rank-languages-by-popularity/Rank-languages-by-popularity.jsexe/

default:all

COPY_FILES = index.html style.css
DEST_FILES = ${addprefix ${OUTPUT_DIR}, ${COPY_FILES} }

${DEST_FILES} : ${OUTPUT_DIR}%: %
	cp $< $@

cabalbuild: 
	cabal build

all: cabalbuild ${DEST_FILES}

