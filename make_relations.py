import feather
import pandas as pd
import numpy as np
import collections
import os
import sys
from spacy.en import English
import ujson
import make_svo
from itertools import chain

nlp = English()
relations = ['amod','conj']
def svo_relations(text, sid):
	sent_id = []
	relation = []
	arg1 = []
	arg2 = []

	text = nlp(text)
	svos = [make_svo.lemmaFindSVOs(s) for s in text.sents]
	svs = [make_svo.lemmaFindSVs(s) for s in text.sents]
	svs = list(chain.from_iterable(svs)) # flatten the list of lists
	svos = list(chain.from_iterable(svos))
	for rel in svos:
		sent_id.append(sid)
		relation.append("object")
		arg1.append(rel[2])
		arg2.append(rel[1])

	for rel in svs:
		sent_id.append(sid)
		relation.append("subject")
		arg1.append(rel[0])
		arg2.append(rel[1])
	
	for tok in text:
		if tok.dep_ == 'amod':
			sent_id.append(sid)
			relation.append("adjective")
			arg1.append(tok.head.lemma_)
			arg2.append(tok.lemma_)

		if tok.dep_ == 'conj':
			sent_id.append(sid)
			relation.append("conjunction")
			arg1.append(tok.head.lemma_)
			arg2.append(tok.lemma_)
	df = pd.DataFrame({"sent_id":sent_id, "relation":relation, "arg1":arg1, "arg2":arg2})
	return(df)


def get_features(text, sid):
	sent_id = []
	feature = []

	text = nlp(text)
	svos = [make_svo.lemmaFindSVOs(s) for s in text.sents]
	svs = [make_svo.lemmaFindSVs(s) for s in text.sents]
	svs = list(chain.from_iterable(svs)) # flatten the list of lists
	svos = list(chain.from_iterable(svos))

	for rel in svos:
		sent_id.append(sid)
		feature.append('object_'+rel[2]+"_"+rel[1])

	for rel in svs:
		sent_id.append(sid)
		feature.append('subject_'+rel[0]+"_"+rel[1])
	
	for tok in text:
		if tok.dep_ == 'amod':
			sent_id.append(sid)
			feature.append('adj_'+tok.head.lemma_+"_"+tok.lemma_)

		if tok.dep_ == 'conj':
			sent_id.append(sid)
			feature.append('conj_'+tok.head.lemma_+"_"+tok.lemma_)

	df = pd.DataFrame({"sent_id":sent_id, "feature":feature})
	return(df)







tmpdf = feather.read_dataframe('corpora/uk_sents.csv')
sents = tmpdf[['sentenceid','sentence_text']]
texts = sents["sentence_text"].tolist()
ids = sents["sentenceid"].tolist()
texts[0:3]
ids[0:3]


end = len(texts)
df = pd.DataFrame({'sent_id':[], 'feature':[] })
for i in range(0,end):
	print(i)
	df = pd.concat([df,get_features(texts[i], ids[i])])
print(df.head(20))
print('writing..')
feather.write_dataframe(df, 'corpora/uk_features.feather')


exit()

end = len(texts)
df = pd.DataFrame({'sent_id':[], 'relation':[], 'arg1':[], 'arg2':[] })
for i in range(0, end):
	print(i)
	df = pd.concat([df,svo_relations(texts[i], ids[i])])
print(df.head(20))
print('writing..')
feather.write_dataframe(df, 'corpora/uk_relations.feather')




