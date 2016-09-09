
(2016/9/9)

* 論文作成に用いた計算コード

CorrelationFunction.c
 : Pearson相関係数を計算する関数
   アップコンバージョンノイズ、地面振動、アップコンバージョンが入った主干渉計信号のノイズデータを同時に計算する関数
   アップコンバージョンノイズ、地面振動、アップコンバージョンが入った主干渉計信号のノイズデータを同時に計算してホワイトニングする関数
   データのホワイトニングを行う関数
   KAGALI関数がいくつも使われているが、基本的に乱数生成や補間などを内部で行っているだけ

CorrelationFunction.h
 : 上記コードのhearder file

parameters.h
 : ノイズ生成を行うときのパラメーターがまとめてある
 サンプリングレート、データ長さ、アップコンバージョンノイズのパラメーター(光学台の共振周波数、その減衰率)
 サイトの地面振動のスペクトルの係数


gene_data.c
 : アップコンバージョンノイズ、地面振動、アップコンバージョンが入った主干渉計信号のノイズデータ1つ書き出す
 (ホワイトニングはしていない)

gene_data_white_sf.c
 : (ホワイトニングと名前についているが、ホワイトニングされていない)周波数スペクトル生成する
   generateAllDataFine のようにFineと入っているものは、乱数を振ったVirgoのnoise budgetのスペクトルをテキスト出力する
   FIG2に対応

gene_data_white.c
 : ホワイトニングされた時系列データを生成する
   FIG3に対応

simulate_white_mic.c
 : ホワイトニングされたアップコンバージョンノイズを生成し、それに対してMICをかける
   最初に鏡が揺れる振幅A_m は実行時の引数
   i_simは何回実行するか
   これを走らせて、A_m=0との比較をすることでFIG5を得た

simulate_white_pearson.c
 : ホワイトニングされたアップコンバージョンノイズを生成し、それに対してPearsonをかける
   最初に鏡が揺れる振幅A_m は実行時の引数
   i_simは何回実行するか
   これを走らせて、A_m=0との比較をすることでFIG5を得た   



* MICパッケージ

ここから入手できる mine を用いた
http://minepy.sourceforge.net/docs/0.3.5/index.html

ソースコードのダウンロード
用いたversionは 1.0.0
mine.h内でLIBMINE_VERSION "1.0.0"となっていた
https://sourceforge.net/projects/minepy/files/

Cコードの使い方説明
http://minepy.sourceforge.net/docs/0.3.5/c.html#c-api



* Virgoのnoise budget
http://w3-1.virgo-gw.eu/senscurve/
にある「Sensitivity Curve Data File」を用いた

gene_mod_noise.sh
 : 上記Data Fileを解凍したあとで、Virgo10.datから必要なnoise budgetデータのみを加工するスクリプト
   mod4_noise.txt というデータが今回用いたnoiseデータ

