for f in $(<listNCT.txt)
do
mongoimport --db aci --collection ClinicalTrials --type json --file /Users/shivanikapadia/Documents/trial_browse_app/data/trials/$f --jsonArray
done
