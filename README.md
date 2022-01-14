# data-cleaner
This code seeks to aid in the data cleaning process by removing empty columns as well as rows (if any) from the dataset and placing them in csv files to alert the user

The function contains three parameters:
  - The directory of the dataset
  - The dataset
  - An option to remove the first row or not
  
In the event the dataset in use has double headings, this feature will be useful in removing it such that it does not interfere with the process

Upon running the function, the user would receive csv files:
  - A cleaned version of the data (in the event there were empty rows and columns)
  - A CSV file containing the empty rows (if any)
  - A CSV file containing the empty columns (if any)


Enjoy!
