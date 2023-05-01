for f in $(<listNCT.txt)
do
mongoimport --db aci --collection ClinicalTrials --type json --file /mnt/cancergenomics/shivani/shiny_repo/trial_browse_app/data/trials/$f --jsonArray
done
