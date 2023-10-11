ls data_get/* | while read line ; do cuenta=$( grep '//' $line | wc -l);echo $cuenta $line; done | sort -n > list.txt
cat list.txt | head -$(($(wc -l < list.txt) * 25 / 100)) | cut -d " " -f 2 > lista.txt
rm list.txt
mkdir subset_25percent
cat lista.txt | xargs -I{} mv {} subset_25percent/
