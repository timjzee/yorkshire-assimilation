Read Table from comma-separated file: "/Users/tim/Documents/sound_files/YA_patrycja/dataset_TiN.csv"
Append column: """voicing_proportion"""
Append column: """n_frames"""

num_rows = Get number of rows
for row from 1 to num_rows
    selectObject: "Table dataset_TiN"
    pp_num = Get value: row, """participant"""
    if pp_num < 10
        pp_lab$ = "W0" + string$(pp_num)
    else
        pp_lab$ = "W" + string$(pp_num)
    endif
    wrd$ = Get value: row, """word"""
    f_name$ = pp_lab$ + "_" + replace$(wrd$, "#", "_", 0)
    vowel_l$ = Get value: row, """vowel_length"""
    if vowel_l$ != "NA"
        appendInfoLine: f_name$
        Read from file: "/Users/tim/Documents/sound_files/YA_patrycja/main/" + pp_lab$ + "/" + f_name$ + ".wav"
        Convert to mono
        Read from file: "/Users/tim/Documents/sound_files/YA_patrycja/main/" + pp_lab$ + "/" + f_name$ + ".TextGrid"
        vowel_start = Get start time of interval: 1, 2
        vowel_end = Get end time of interval: 1, 2
        cons_start = Get start time of interval: 1, 3
        cons_end = Get end time of interval: 1, 3
        vowel_length = vowel_end - vowel_start
        cons_length = cons_end - cons_start
        selectObject: "Sound " + f_name$ + "_mono"
        To Pitch (cc): 0, 75, 15, "yes", 0.03, 0.60, 0.01, 0.35, 0.14, 600
        runScript: "/Users/tim/Library/Preferences/Praat Prefs/plugin_PraatZee/extractPeriodicity.praat", cons_start, cons_end, "no"
        voiced_frames = Count voiced frames
        total_frames = Get number of frames
        voicing_prop = voiced_frames / total_frames
        selectObject: "Table dataset_TiN"
        Set string value: row, """vowel_length""", string$(vowel_length)
        Set string value: row, """cons_length""", string$(cons_length)
        Set string value: row, """ratio""", string$(cons_length / vowel_length)
        Set string value: row, """voicing_proportion""", string$(voicing_prop)
        Set string value: row, """n_frames""", string$(total_frames)
        removeObject: "Sound " + f_name$
        removeObject: "Sound " + f_name$ + "_mono"
        removeObject: "TextGrid " + f_name$
        removeObject: "Pitch " + f_name$ + "_mono"
        removeObject: "Pitch " + f_name$ + "_mono_part"
    else
        selectObject: "Table dataset_TiN"
        Set string value: row, """voicing_proportion""", "NA"
        Set string value: row, """n_frames""", "NA"
    endif
endfor

Save as comma-separated file: "/Users/tim/GitHub/yorkshire-assimilation/dataset_voicing.csv"
