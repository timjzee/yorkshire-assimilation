subt_path = "/Users/tim/Documents/sound_files/YA_patrycja/"
data_path = "/Users/tim/GitHub/yorkshire-assimilation/"

subtlex = {}
with open(subt_path + "SUBTLEX-UK.txt", "r") as f:
    for l in f:
        l_list = l.split("\t")
        word = l_list[0]
        log_freq = l_list[5]
        subtlex[word] = log_freq

with open(data_path + "dataset_voicing.csv", "r") as f:
    with open(data_path + "dataset_voicing2.csv", "w") as g:
        for c, l in enumerate(f, 1):
            if c == 1:
                l2 = l[:-1] + ",w1_freq,w2_freq\n"
            else:
                l_list = l[:-1].split(",")
                w1, w2 = l_list[1].split("#")
                w1_f = subtlex.get(w1, "NA")
                w2_f = subtlex.get(w2, "NA")
                l2 = "{},{},{}\n".format(l[:-1], w1_f, w2_f)
            g.write(l2)
