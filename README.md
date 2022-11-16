# CPBL
2019 CPBL Sabermetrics project<br>
Project Members: 黃嘯風、黃振維、劉昱呈、陳冠澤、陳竑瑋、黃文樂 <br>
Project Advisor: Prof. 黃俊堯 <br>
Project Presentation: https://tinyurl.com/msazmwry<br>

資料處理

1:由log文字敘述判斷壘上幾人、幾出局、有無得分 

2:計算Win Expectancy(由14-19年1220場做歷史資料)，並先由兩行加減算出wpa

3:判斷先發、後援投手名字，判斷換投時機，新增投手欄

4:將no.2算完的wpa做修正

5:修正打者欄及球隊欄會遇到的bug

6:將每打席rem type、局數、分差對應到該打席的leverage index


初步分析

7:打者wpa分析

8:投手wpa分析、WPA Win-Loss

9:clutch分析

10:中職14-19戰績處理

log2data.R :轉換logfiles到csv
