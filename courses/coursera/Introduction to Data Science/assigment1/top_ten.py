import sys
import re
import json

# from pprint import pprint

from collections import Counter
count = Counter()

def for_each_tweet(tweet):
    if 'entities' in tweet and tweet['entities'] != None:
        entities = tweet['entities']
        if 'hashtags' in entities and entities['hashtags'] != []:
            # pprint(entities)
            hashes = entities['hashtags']
            count.update([e['text'] for e in hashes if 'text' in e])

tweet_file = open(sys.argv[1])
line = tweet_file.readline()
while line:
    tweet = json.loads(line)
    for_each_tweet(tweet)
    line = tweet_file.readline()
tweet_file.close()


for word, count in count.most_common(10):
    print word, count