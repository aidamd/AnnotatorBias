import pandas as pd
from collections import Counter
import re

"""
adds the sgt and agreement scores for each post
"""

def fleiss(labels):
    counts = Counter(labels)
    return (choose_2(counts[0]) + choose_2(counts[1])) / choose_2(len(labels))

def choose_2(x):
    return x * (x - 1) / 2

def vote(labels):
    if Counter(labels)[1] >= Counter(labels)[0]:
        return 1
    else:
        return 0

SGTs = [x.replace("\n", "") for x in
            open("../../HateSpeech/extended_SGT.txt", "r").readlines()]
annotations = pd.read_csv("Data/annotations_id.csv")
posts_fleiss = {col: list() for col in ["id", "text", "hate", "agreement", "sgts", "has_sgt", "annotators"]}

for name, group in annotations.groupby(["tweet_id"]):
    posts_fleiss["id"].append(name)
    posts_fleiss["agreement"].append(fleiss(group["hate"].tolist()))
    posts_fleiss["hate"].append(vote(group["hate"].tolist()))
    text = group["text"].tolist()[0]
    posts_fleiss["text"].append(text)
    posts_fleiss["annotators"].append(group.shape[0])
    sgts = list()
    for sgt in SGTs:
        if re.findall("((^|[^\w])(" + sgt + ")([^\w]|$|s))", text.lower()):
            sgts.append(sgt)
    if sgts:
        posts_fleiss["has_sgt"].append(1)
    else:
        posts_fleiss["has_sgt"].append(0)
    posts_fleiss["sgts"].append(sgts)

pd.DataFrame.from_dict(posts_fleiss).to_csv("Data/posts_fleiss.csv", index=False)