# player-forecasting-with-Prophet-thesis
This repository contains the R code and dataset used for my Bachelor's Thesis in Statistics for Technology and Science at University of Padua, written in italian, which focuses on using the Prophet model to forecaste the number of active players in the game Rocket League and estimating the optimal number of servers required to ensure stable performance and cost-effective scalability.

## Dataset
The dataset 252950.csv used in this thesis was taken from the platform [Select Dataset](https://www.selectdataset.com/dataset/71894030462cb535efc96a766e6bb83e) and it covers the number of active players in *Rocket League* from December 2017 to August 2020.  


## Abstract

Online gaming is undergoing a phase of strong expansion, driven by increasing connectivity, the spread of cross-play platforms, and advancements in cloud technologies. In this context, the ability to effectively scale server infrastructures is essential to ensure stable performance and a smooth gaming experience, even during traffic peaks. Optimal resource management therefore represents a strategic challenge for companies in the gaming industry.

This thesis focuses on forecasting the number of active players on Rocket League, one of the most representative multiplayer titles, with the goal of effectively estimating the number of servers needed to meet demand while balancing operational costs and service quality. To this end, a high-frequency dataset covering the period from December 2017 to August 2020 is used, allowing for a detailed analysis of demand trends across different time scales.

The Prophet model is employed to generate forecasts over hourly, weekly, and monthly horizons, thereby supporting both short-term and long-term strategic decisions. The analysis also considers the impact of extraordinary events, such as the COVID-19 pandemic, by evaluating scenarios where such shocks are either included or excluded from the predictive model, to understand their influence on forecast accuracy. The results obtained with Prophet are then compared to those of the ARIMA model, to analyze differences in terms of accuracy and adaptability.

The work concludes with an estimate of the optimal number of servers to keep active based on the predicted demand, showing how modeling results can support informed decision-making. The aim is to offer a replicable methodological framework for optimizing scalability in online gaming environments, contributing to smarter and more efficient resource management.
