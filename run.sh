#!/bin/bash

# Utility script to speed up the process of converting from md to pdf using
# ibidem in the way

# Name of the program executable
EXECUTABLE_NAME=ibidem

# Path where the executable was build
BUILD_PATH="./build"

# Path where the example lies
EXAMPLE_PATH="./example"

# The input .md file
MD_FILE="$EXAMPLE_PATH/article.md"

# The output .tex file
LATEX_FILE="$BUILD_PATH/article.tex"

# An optional layout file
LAYOUT_FILE="$EXAMPLE_PATH/layout.tex"

# Output of intermediary files of pdflatex
OUTPUT_DIR="$BUILD_PATH/output"

mkdir -p $OUTPUT_DIR
$BUILD_PATH/$EXECUTABLE_NAME $MD_FILE -o $LATEX_FILE -l $LAYOUT_FILE
pdflatex -output-directory $OUTPUT_DIR $LATEX_FILE
cp $OUTPUT_DIR/article.pdf $BUILD_PATH

