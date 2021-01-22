"""
This file removes annotators who did not write proper text for the open ended questions
"""

import argparse
import pandas as pd
import spacy
import glob
import os

def open_ended(df):
    cols = ["whatismorality", "immoralbyyou", "immoraltoyou"]
    start = False
    for i, row in df.iterrows():
        if not start:
            if row["rid"] == "5f28311b-9a42-1b2c-633e-fd0ab3e08bcd":
                start = True
            else:
                continue
        for col in cols:
            if isinstance(row[col], str) and len(row[col]) > 10:
                print(row[col])

def real_word(text):
    doc = nlp(text)

def concat(dir):
    for i, file in enumerate(glob.glob(dir + "/Annotators*")):
        df = pd.read_csv(file)
        print(df.shape)
        if i == 0:
            df.to_csv(os.path.join(dir, "Annotators_Love_Mohammad_correct.csv"), index=False)
        else:
            df.to_csv(os.path.join(dir, "Annotators_Love_Mohammad_correct.csv"), index=False, header=False, mode="a")
    df = pd.read_csv(os.path.join(dir, "Annotators_Love_Mohammad_correct.csv"))
    df = df.drop_duplicates(subset=["ResponseId"])
    df = df.reset_index()
    drop = []
    for i, row in df.iterrows():
        if row["gc"] not in [1, "1"]:
            drop.append(i)
    df = df.drop(drop)

    df.to_csv(os.path.join(dir, "Annotators_Love_Mohammad_correct.csv"), index=False)
    print("final number of participants", df.shape[0])
    return df

def check_hate(df):
    df = df.loc[(df["Test_1"] == "1") &
                (df["Test_2"] == "1") &
                (df["Test_3"] == "0"),]
    print(df.shape[0], "participants correctly labeled the test items")
    return df

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--dir")

    args = parser.parse_args()
    try:
        df = pd.read_csv(os.path.join(args.dir, "Annotators_Love_Mohammad_correct.csv"))
    except Exception:
        df = concat(args.dir)

    exit(1)
    nlp = spacy.load("en")
    open_ended(df)



