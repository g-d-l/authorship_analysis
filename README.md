This program guesses the author of an unknown text based on a corpus of sample texts with known authors. The main methods used are a comparison of the top ngrams (via Hirschberg's algorithm) and other similar metrics, including word length, conjunction usage, etc. 

# To Compile and Run:

1. Enter `make` in the command line to compile
2. Run the program via:

  `./Main.native FILENAME from:YEAR1 to:YEAR2`

- filename is the file path to the mystery text you want to test
- `YEAR1` and `YEAR2` are 4 digit integers, the range in which you think your
- unknown text is from. These are each optional, but the from: and to: tags
- must be included

For example, try these 2 sample "mystery" texts:

`./Main.native texts/sample_Northranger_Abbey.txt from: to:`

`./Main.native texts/sample_Romeo_and_Juliet.txt from: to:`

`./Main.native texts/sample_Romeo_and_Juliet.txt from:1500 to:1650`


# Settings:

The index file within the text's folder is an index of the preloaded texts for the database. To add or remove, simply follow the given format, and the program will reflect the changes.


Settings can be changed in `Settings.ml`:
- the ngrams size can be anywhere from 2 to 10 or so, though
 3 to 6 tends to work best
- if you text has a lot of long words, `max_word_length` should be
 increased for better results
- to compare more of the top ngrams for each author, change `top_ngram_length`
- to change the constant memory used for the Hirschberg base case, change
 `hirsch_mem_lim`. Generally, values at least 4^2 = 16 work best. Of course, 
 going beyond `top_ngram_length`^2 is useless.
