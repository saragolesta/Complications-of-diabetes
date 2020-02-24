This project was done in two languages R and Python.
To read the dataset have the diabetic_data.csv in the same folder as the Preprocess.ipynb file. Preprocess.ipynb cleans the data and generates two files: preprocessed.csv and combined.csv.

combined.csv is used by EDA.Rmd for Exploratory Data Analysis.
Preprocessed.csv only contains the features and is used by Oversample.ipynb and Undersample.ipynb for training and evaluating the models.

So to run the notebooks, run Preoprocess.ipynb before other notebooks.

Note that for Oversample and Undersample.ipynb, you may need to run the follwing commands to install the additional libraries:

pip install imblearn
pip install xgboost
pip install shap