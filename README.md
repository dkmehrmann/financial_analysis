# financial_analysis

### Personal Finance Analytics for Chase Transaction Data

Chase now allows you to download (up to three months of) your transaction history. `data_prep.R` will organize the data as well as take command-line input to categorize businesses in a way that makes sense for you. For example, I like to use the categories **restaurants**, **grocery stores**, **retail**, **online**, etc. to view my financial behavior over time. The code is flexible so you can choose your own categories, and creates a mapping of businesses to categories so you do not have to categorize each business every time you pull new data from Chase.

The Shiny app currently creates a plot of total spending over time as well as a grouped line plot of spending by category over time. 

## To Do

There is currently a "bug" where rare categories get plotted incorrectly because the weeks without transactions in those categories are not resampled and filled with 0. For example, if I make an online purchase in June of $20 and one in August of $40, this will appear as a steady climb from June to August rather than a spike in June then a Spike in August. 


