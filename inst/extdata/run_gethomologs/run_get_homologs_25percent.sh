cat lineagelist.txt | while read line; do mkdir -p $line/data_get $line/data_get_25percent; ln -s /home/andres/families_DB/$line/02*/*.gbk $line/data_get; cd $line; ls data_get/* | while read line ; do cuenta=$( grep '//' $line | wc -l);echo $cuenta $line; done | sort -n > list.txt
cat list.txt | head -$(($(wc -l < list.txt) * 25 / 100)) | cut -d " " -f 2 > lista.txt
rm list.txt
cat lista.txt | xargs -I{} mv {} data_get_25percent/; get_homologues.pl -n 20 -d data_get_25percent -t 0 -M; get_homologues.pl -n 20 -d data_get_25percent -t 0 -G; dirs=$(ls -d data_get_25percent_homologues/*0taxa*/ | tr '\n' ',' | sed 's/,$//'); compare_clusters.pl -o alg_intersection -m -d $dirs;cd ..; done
