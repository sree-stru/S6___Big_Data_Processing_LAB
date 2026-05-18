# MAPPER PROGRAM

#!/usr/bin/env python3
import sys
# Read input line by line from standard input
for line in sys.stdin:
    # Remove leading/trailing spaces
    line = line.strip()
    # Split line into words
    words = line.split()
    # Output each word with count 1
    for word in words:
        print(f"{word}\t1")
      
# REDUCER PROGRAM

#!/usr/bin/env python3
import sys
current_word = None
current_count = 0
# Read mapper output
for line in sys.stdin:
    # Split word and count
    word, count = line.strip().split("\t", 1)
    # Convert count to integer
    count = int(count)
    # If same word, add counts
    if current_word == word:
        current_count += count
    else:
        # Print previous word count
        if current_word:
            print(f"{current_word}\t{current_count}")
        # Start counting new word
        current_word = word
        current_count = count
# Print last word
if current_word:
    print(f"{current_word}\t{current_count}")
