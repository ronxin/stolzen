import sys
import re
import json

def sentiment(sentiment_filename):
    afinnfile = open(sentiment_filename)
    scores = {}
    for line in afinnfile:
        term, score  = line.split("\t") 
        scores[term] = int(score)
    afinnfile.close()
    return scores

def split_by_words(line):
    return re.findall("[^\s]+", line)

def calc_sentiment_tweet(line, sent_dic):
    words = split_by_words(line)
    s = 0
    # print words,
    for word in words:
        if word in sent_dic:
            s = s + sent_dic[word]
    return s

def calc_sentiment_file(tweet_file, sent_dic):
    line = tweet_file.readline()
    while line:
        tweet = json.loads(line)
        sent = calc_sentiment_tweet(tweet['text'], sent_dic)
        print sent
        line = tweet_file.readline()


def main():
    sent_file = sys.argv[1]
    tweet_file = open(sys.argv[2])
    sent_dic = sentiment(sent_file)
    calc_sentiment_file(tweet_file, sent_dic)


if __name__ == '__main__':
    main()
