import csv
import re

listfname = "list.csv"
listStudent = []
with open(listfname, newline='') as csvfile:
    spamreader = csv.reader(csvfile)#, delimiter=' ', quotechar='|')
    for row in spamreader:
        listStudent.append(row)

# print(listStudent)

#kadaiSet = ['p01', ["reidai2_1(sai)*.c"]]
#kadaiSet = ['p02', ["reidai2_2a(sai)*.c", "reidai2_2b(sai)*.c"]]
#kadaiSet = ['p03', ["reidai2_3a(sai)*.c", "reidai2_3b(sai)*.c"]]
#kadaiSet = ['p04', ["reidai3_1a(sai)*.c", "reidai3_1b(sai)*.c", "reidai3_1c(sai)*.c"]]
#kadaiSet = ['p05', ["reidai3_2(sai)*.c", "kadai5(sai)*.c"]]
#kadaiSet = ['p06', ["reidai3_3(sai)*.c", "kadai6(sai)*.c"]]
#kadaiSet = ['p07', ["reidai4_1a(sai)*.c", "reidai4_1b(sai)*.c", "reidai4_1c(sai)*.c", "kadai7(sai)*.c"]]
#kadaiSet = ['p09', ["reidai4_2(sai)*.c", "kadai9a(sai)*.c", "kadai9b(sai)*.c", "kadai9c(sai)*.c"]]
#kadaiSet = ['p10', ["reidai4_3a(sai)*.c", "reidai4_3b(sai)*.c", "kadai10a(sai)*.c", "kadai10b(sai)*.c"]]
#kadaiSet = ['p11', ["reidai5_1(sai)*.c", "reidai5_2(sai)*.c", "kadai11(sai)*.c"]]
#kadaiSet = ['p12', ["reidai6_1(sai)*.c", "kadai12a(sai)*.c", "kadai12b(sai)*.c"]]
#kadaiSet = ['p13', ["reidai6_2(sai)*.c", "kadai13a(sai)*.c", "kadai13b(sai)*.c"]]
#kadaiSet = ['p14', ["reidai7_2a(sai)*.c", "reidai7_2b(sai)*.c", "kadai14a(sai)*.c"]]
kadaiSet = ['p15', ["reidai8_1(sai)*.c", "kadai15a(sai)*.c", "kadai15b(sai)*.c"]]

fname = kadaiSet[0]
prefixes = kadaiSet[1]

ls = []

f = open(fname, "r")
for a in f:
    ls.append(a.strip()) # strip()で改行を削除
f.close()

for prefix in prefixes:
    listStudent[0].append(prefix)

for i in range(1,len(listStudent)):
    for prefix in prefixes:
        result = [l for l in ls if re.search(listStudent[i][2]+prefix,l) != None]
        if len(result) > 0:
            listStudent[i].append(1)
        else:
            listStudent[i].append(0)
        ls = [l for l in ls if l not in result]
        #print(result)

with open('out.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerows(listStudent)

    writer.writerow(["Remain"])
    for l in ls:
        writer.writerow([l])

