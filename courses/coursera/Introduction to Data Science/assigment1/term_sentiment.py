import sys
import re
import json

def sentiment(sent_file):
    scores = {}
    for line in sent_file:
        term, score  = line.split("\t") 
        scores[term] = int(score)
    return scores

def split_by_words(line):
    return re.findall("[^\s]+", line)

def solve(sent_file, tweet_file):
    sent_dic = sentiment(sent_file)
    sent_file.close()

    new_words = {}

    def calc_sentiment(tweet):
        words = split_by_words(tweet)
        s = 0
        for word in words:
            if word in sent_dic:
                s = s + sent_dic[word]
        return s

    def append_new_words(tweet):
        s = calc_sentiment(tweet)
        words = split_by_words(tweet)
        for word in words:
            if not word in sent_dic:
                if word in new_words:
                    new_words[word] = new_words[word] + s
                else:
                    new_words[word] = s

    def for_each_tweet(text):
        append_new_words(text)

    def print_out_results():
        for k, v in new_words.items():
            try:
                print k, v
            except UnicodeEncodeError, e:
                continue

    def calc_sentiment_file(tweet_file):
        line = tweet_file.readline()
        while line:
            tweet = json.loads(line)
            if 'text' in tweet:
                for_each_tweet(tweet['text'])
            else:
                for_each_tweet('')
            line = tweet_file.readline()


    calc_sentiment_file(tweet_file)
    tweet_file.close()
    print_out_results()


def main():
    sent_file = open(sys.argv[1])
    tweet_file = open(sys.argv[2])
    solve(sent_file, tweet_file)

if __name__ == '__main__':
    main()
