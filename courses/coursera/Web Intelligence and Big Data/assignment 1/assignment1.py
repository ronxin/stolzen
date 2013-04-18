#!/usr/bin/env python


# map and reduce functions

def mapfn(key, value):
    from stopwords import allStopWords
    import re
    import itertools

    def process_paper(line):
        tokens = line.split(':::')
        paper_id = tokens[0]
        authors = tokens[1].split('::')
        title = tokens[2]
        return paper_id, authors, title

    def good_word(word):
        return (not word in allStopWords) and len(word) > 1

    def preprocess_title(title):
        words = re.split('[,.!?:;%*]?\s+|[,.!?;%*]$', title)
        good_words = filter(good_word, words)
        return map(lambda w: w.lower(), good_words)

    print "mapfn: processing %s" % key
    for line in value.splitlines():
        paper_id, authors, title = process_paper(line)
        words = preprocess_title(title)
        for (author, words) in itertools.product(authors, words):
            yield author, words


def reducefn(key, value):
    from collections import Counter
    # print "reducefn: processing %s (values: %s)" % (key, value)
    count = Counter(value)
    return key, count.most_common(2)