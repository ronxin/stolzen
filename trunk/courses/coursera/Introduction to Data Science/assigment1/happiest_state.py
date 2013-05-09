import sys
import re
import json


def sentiment(sent_file):
    scores = {}
    for line in sent_file:
        term, score  = line.split("\t") 
        scores[term] = int(score)
    return scores


def main():
    def extract_state(tweet):
        if 'place' in tweet and tweet['place'] != None:
            place = tweet['place']
            if place['country_code'] == u'US': 
                state = place['full_name'][-2:]
                if state.isupper():
                    return state
        return None

    def split_by_words(line):
        return re.findall("[^\s]+", line)

    def calc_sentiment_tweet(line):
        words = split_by_words(line)
        s = 0
        for word in words:
            if word in sent_dic:
                s = s + sent_dic[word]
        return s

    from collections import Counter
    states_happiness = Counter()

    def for_each_tweet(tweet):
        state = extract_state(tweet)

        if state != None and 'text' in tweet:
            s = calc_sentiment_tweet(tweet['text'])
            states_happiness[state] += s

    sent_file = open(sys.argv[1])
    sent_dic = sentiment(sent_file)
    sent_file.close()
    tweet_file = open(sys.argv[2])

    line = tweet_file.readline()
    while line:
        tweet = json.loads(line)
        for_each_tweet(tweet)
        line = tweet_file.readline()
    tweet_file.close()

    state, _ = states_happiness.most_common(1)[0]
    print state

if __name__ == '__main__':
    main()
