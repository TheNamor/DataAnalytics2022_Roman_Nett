
import csv

def parse_data(filename):
    # Parses csv file into dataset
    out = []
    with open(filename) as csvfile:
        data = csv.reader(csvfile, delimiter=",")
        for row in data:
            out.append(row)
    return out

data = parse_data("water_sanitation.csv")

levels = {
    "Safely managed service": 1,
    "At least basic": 0.8,
    "Basic service": 0.6,
    "Limited service": 0.4,
    "Unimproved": 0.2,
    "Surface water": 0
}

scores = dict()

for row in data[1:]:
    if row[0] not in scores:
        scores[row[0]] = 0
    scores[row[0]] += float(row[5])*levels[row[7]]

with open("sanitation.csv", "w") as outfile:
    out_text = "iso,SCORE\n"
    for item in scores:
        out_text += f"{item},{scores[item]}\n"
    
    outfile.write(out_text)