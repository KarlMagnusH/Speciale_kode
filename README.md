# Speciale_kode
The repository contain scripts used for my master thesis. The thesis is about the price earned by non-dispatchable produceres of electricity (Wind and solar) in Denmark i the period from 2015 to 2020. 

The scripts contain a mix of data extration (ENTSO_E_data_pull and weather_data), cleaning and visualization (Samling_af_data) and running the linear 2 stage least squares instrumental variable estimation (Regression). The estimations are made for multiple robustness checks and also contain visualization.

The order of running the scripts should be as follow:
1. ENTSO_E_data_pull which takes about 4 days. In order to run the script one must change the folder paths and get acess to their own API key form ENTSO-E
2. weather_data which take about 15min to run. In order to run the script one must change the folder paths and get acess to their own API key form DMI (the script contains links and a guide). In case you want acess to big amounts of data i recommend using the bulk files provided by DMI. To extract weather data you can use the script also provided in the reposity called extract_weather_data_from_bulk. 
3. Samling_af_data, one must change the folder paths for the script to run
4. Regression, one must change the folder paths for the script to run

You are welcome to reach out to me. 
