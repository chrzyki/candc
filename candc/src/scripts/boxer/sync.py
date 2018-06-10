import re, sys
import os

def syncone(RTE_dir):

    tccg = RTE_dir + "/t.ccg"
    hccg = RTE_dir + "/h.ccg"
    htemp = RTE_dir + "/h.ccg.temp"
    if not (os.path.exists(tccg) and os.path.exists(hccg)):
        print "ERROR: no CCG file in " + RTE_dir
        return 0
    fo = open(htemp,"wb")

    tn = []

    for line in open(tccg):
        pred = line.rstrip('\n')
        pattern = re.compile(r"t\((n(/n)?), (.*), (.*), ('NN(.)?'), (.*), ('.*').*")
        m = pattern.search(pred)
        if m:
            hash = {}
            hash['category'] = m.group(1)
            hash['word'] = m.group(3)
            hash['lemma'] = m.group(4)
            hash['tag1'] = m.group(5)
            hash['tag2'] = m.group(7)
            hash['tag3'] = m.group(8)
            tn.append(hash)
            
    change = 0
    for line in open(hccg):
        pred = line.rstrip('\n')
        pattern = re.compile(r"t\((n(/n)?), (.*), (.*), ('NN(.)?'), (.*), ('.*').*")
        m = pattern.search(pred)
        if m is None:
            fo.write(pred + "\n")
        else:
            word = m.group(3)
            match = 0
            replaced = 0
            for t in tn:
                #if the tag is the same then do nothing
                if m.group(3) == t['word'] and m.group(8) == t['tag3']:
                    match = 1
                    break
            if match is 0:
                for t in tn:
                    if t['word'] == word:
                        pred = pred.replace(m.group(4),t['lemma'])
                        pred = pred.replace(m.group(5),t['tag1'])
                        pred = pred.replace(m.group(7),t['tag2'])
                        pred = pred.replace(m.group(8),t['tag3'])
                        fo.write(pred + "\n")
                        replaced = 1
                        change = 1
                        break
            if replaced is 0:
                fo.write(pred + "\n")
    fo.close()

#    if change > 0:
#        print RTE_dir + " changed"
#        os.rename(htemp,hccg)
#        command = "%s/bin/boxer --input %s --output %s --resolve true --plural false --modal true --vpe false --roles proto --copula false --elimeq true --warnings false --box" % (candc,hccg,RTE_dir + "/h.drs")
#        os.system(command)
#    else:
    os.remove(htemp)
    return change


def syncall(RTE_dir):
    changes = 0
    for pairs in os.listdir(RTE_dir):
        RTE_pairs = RTE_dir + "/" + pairs
        ch = syncone(RTE_pairs)
        changes = changes + ch
    return changes



#parser = argparse.ArgumentParser(description='Synchronisation',add_help=True)

#parser.add_argument('--all', action="store_true", default=False, dest="processall",help="Process all RTE pairs")
#parser.add_argument('--candc', action="store", default="/home/annisa/candc", dest="candcdir",help="root dir of candc")
#parser.add_argument('--dir', action="store", dest="dir",help="RTE dir")

#if len(sys.argv)==1:
#    parser.print_help()
#    sys.exit(1)

#arguments = parser.parse_args()
#candc = arguments.candcdir
#RTE_dir = arguments.dir
RTE_dir = "/net/aistaff/bos/candc/working/rte/all"
all = True

#remove trailing slash
#if candc[-1:] == "/":
#    candc = candc[0:-1]
if RTE_dir[-1:] == "/":
    RTE_dir = RTE_dir[0:-1]

ch = 0
if all:
    ch = syncall(RTE_dir)
else:
    ch = syncone(RTE_dir)
print str(ch) + " pairs are synchronised"
