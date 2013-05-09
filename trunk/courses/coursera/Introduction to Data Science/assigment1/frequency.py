import sys
import re
import json


from collections import Counter
count = Counter()

total_num = 0


def split_by_words(line):
    return re.findall("[^\s]+", line)

def for_each_tweet(tweet):
    words = split_by_words(tweet)
    count.update(words)
    global total_num
    total_num = total_num + len(words)

tweet_file = open(sys.argv[1])
line = tweet_file.readline()
while line:
    tweet = json.loads(line)
    if 'text' in tweet:
        for_each_tweet(tweet['text'])
    line = tweet_file.readline()
tweet_file.close()


for word, count in count.items():
    try:
        print "%s %.10f" % (word, float(count) / total_num)
    except UnicodeEncodeError, e:
        continue
