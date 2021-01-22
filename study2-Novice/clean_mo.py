import pandas as pd

"""
use this function before moving to the R code
"""
def get_disagreement():
    a = pd.read_csv("Data/survey_cleaned.csv")
    groups = "Muslim, Jew, Immigrant, Communist, Liberal, Black, Gay, Woman".split(", ")
    maj = dict()
    for group in groups:
        for num in range(1, 8):
            q = group + "_" + str(num)
            maj[q] = 1 if sum(a[q]) > a.shape[0] / 2 else 0

    new_cols = {"disagree_tot": list(),
                "disagree_pos": list(),
                "disagree_neg": list()}

    for group in groups:
        new_cols[group + "_disagree_neg"] = list()
        new_cols[group + "_disagree_pos"] = list()
        new_cols[group + "_disagree"] = list()

    for i, row in a.iterrows():
        pos_tot = 0
        neg_tot = 0
        for group in groups:
            pos = 0
            neg = 0
            for num in range(1, 8):
                q = group + "_" + str(num)
                if row[q] != maj[q]:
                    if maj[q] == 1:
                        neg += 1
                    else:
                        pos += 1
            new_cols[group + "_disagree_neg"].append(neg)
            new_cols[group + "_disagree_pos"].append(pos)
            new_cols[group + "_disagree"].append(neg + pos)
            pos_tot += pos
            neg_tot += neg
        new_cols["disagree_neg"].append(neg_tot)
        new_cols["disagree_pos"].append(pos_tot)
        new_cols["disagree_tot"].append(neg_tot + pos_tot)

    for col in new_cols:
        a[col] = pd.Series(new_cols[col])

    a.to_csv("Data/survey_disagreement.csv", index=False)

def get_SGT_tot():
    a = pd.read_csv("Data/survey_disagreement.csv")
    SGT_hate = list()

    for i, row in a.iterrows():
        if row["SGT"] == "Mexican":
            col = "immigrant"
        else:
            col = row["SGT"].lower()
        SGT_hate.append(row["hate_" + col + "_tot"])

    a["hate_SGT_tot"] = pd.Series(SGT_hate)
    a.to_csv("Data/survey_SGT.csv", index=False)

def get_item_response():
    a = pd.read_csv("Data/survey_SGT.csv")
    cols = ["Immigrant_1", "Immigrant_2", "Immigrant_3", "Immigrant_4", "Immigrant_5", "Immigrant_6", "Immigrant_7",
              "Muslim_1", "Muslim_2", "Muslim_3", "Muslim_4", "Muslim_5", "Muslim_6", "Muslim_7",
              "Communist_1", "Communist_2", "Communist_3", "Communist_4", "Communist_5", "Communist_6", "Communist_7",
              "Liberal_1", "Liberal_2", "Liberal_3", "Liberal_4", "Liberal_5", "Liberal_6", "Liberal_7",
              "Black_1", "Black_2", "Black_3", "Black_4", "Black_5", "Black_6", "Black_7",
              "Gay_1", "Gay_2", "Gay_3", "Gay_4", "Gay_5", "Gay_6", "Gay_7",
              "Jew_1", "Jew_2", "Jew_3", "Jew_4", "Jew_5", "Jew_6", "Jew_7",
              "Woman_1", "Woman_2", "Woman_3", "Woman_4", "Woman_5", "Woman_6", "Woman_7",]
    maj = dict()

    for col in cols:
        maj[col] = 1 if sum(a[col]) > a.shape[0] / 2 else 0
        #print(col)
        #print(maj[col])

    ans = {col: list() for col in cols}
    ans["ResponseId"] = list()
    for i, row in a.iterrows():
        ans["ResponseId"].append(row["ResponseId"])
        for col in cols:
            #cor = 1 if row[col] == maj[col] else 0
            #ans[col].append(cor)
            ans[col].append(row[col])

    pd.DataFrame.from_dict(ans).to_csv("Data/item_response.csv", index=False)

def get_lower(sgt):
    if sgt == "Jew":
        return "jewish"
    else:
        return sgt.lower()

def get_stereo():
    a = pd.read_csv("Data/survey_cleaned.csv")
    df = {"warmth": list(),
          "competence": list(),
          "hate_labels": list(),
          "SGT": list(),
          "user_idx": list(),
          "disagree": list(),
          "disagree_neg": list(),
          "disagree_pos": list()}

    groups = "Muslim, Jew, Immigrant, Communist, Liberal, Black, Gay, Woman".split(", ")

    maj = dict()
    for group in groups:
        for num in range(1, 8):
            q = group + "_" + str(num)
            maj[q] = 1 if sum(a[q]) > a.shape[0] / 2 else 0

    for i, row in a.iterrows():
        for group in groups:
            df["user_idx"].append(i)
            df["warmth"].append(row["SD_" + get_lower(group) + "_warm"])
            df["competence"].append(row["SD_" + get_lower(group) + "_competent"])
            df["SGT"].append(get_lower(group))
            df["hate_labels"].append(sum([row[group + "_" + str(n)] for n in range(1, 8)]))

            pos = 0
            neg = 0
            for num in range(1, 8):
                q = group + "_" + str(num)
                if row[q] != maj[q]:
                    if maj[q] == 1:
                        neg += 1
                    else:
                        pos += 1
            df["disagree_neg"].append(neg)
            df["disagree_pos"].append(pos)
            df["disagree"].append(neg + pos)

    pd.DataFrame.from_dict(df).to_csv("Data/survey_long.csv", index=False)


if __name__ == "__main__":
    get_disagreement()
    get_SGT_tot()
    #get_item_response()
    get_stereo()