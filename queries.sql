INSERT INTO prior_values_verb (verb_phrase, prior) SELECT verb_phrase, COUNT(DISTINCT tweet_no)/(SELECT MAX(tweet_no) from verb) from verb GROUP BY verb_phrase;


INSERT INTO prior_values_noun (noun_phrase, prior) SELECT noun_phrase, COUNT(DISTINCT tweet_no)/(SELECT MAX(tweet_no) from noun) from noun GROUP BY noun_phrase;

SELECT COUNT(*) from ((SELECT DISTINCT(tweet_no) from noun where trim(noun_phrase) = "he") INTERSECT (SELECT DISTINCT(tweet_no) from noun where trim(noun_phrase) = "him") INTERSECT (SELECT DISTINCT(tweet_no) from verb where trim(verb_phrase) = "are"));


SELECT Count(*) from noun as a  INNER JOIN noun as b ON a.tweet_no=b.tweet_no where (trim(a.noun_phrase) = "he") AND trim(b.noun_phrase)='him'

SELECT COUNT(*) from noun as a  INNER JOIN noun as b ON a.tweet_no=b.tweet_no inner join verb as c on b.tweet_no=c.tweet_no where (trim(a.noun_phrase) LIKE "all the leftists") AND (trim(b.noun_phrase) LIKE 'his comments') AND (trim(c.verb_phrase) LIKE "triggered")


#To retrieve tweets
SELECT * from tweets WHERE row_names = (SELECT DISTINCT(a.tweet_no) from noun as a INNER JOIN noun as b ON a.tweet_no=b.tweet_no inner join verb as c on b.tweet_no=c.tweet_no where (a.noun_phrase like " palamedu ") AND (b.noun_phrase like " th february ") AND (c.verb_phrase like " to be conducted "))

#C4 Computation
SELECT * from tweets where data not LIKE "%jallikattu%" AND data not LIKE "%supreme court tomorrow%" AND data not LIKE "%passed%"