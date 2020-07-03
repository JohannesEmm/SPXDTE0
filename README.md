A set of functions for analyzing, logging, and automated trading of SPX DTE0 options strategies.

Requirements:
1) R & Rstudio(recommended) OPEN SOURCE software
2) Interactive Brokers account with TWS software and API enabled
3) A flexquery for your trades (see below).

How to:
Enter your personal details:
Open the file account_data_template.txt and enter the flexquery id and token (see below), as well as the starting date from which you wish to create the log, and your account size in USD.

Run the code:
Just run spxdte0_tradelog.R in the directory with all files (setting the working directory to the folder)

Create your flexquery:
Log into your Interactive Brokers Account and click on Reports -> Flex Queries -> select only "Trades", and "XML" as format, and leave all other options by there default.
Once created, under the "i" symbol you will find the Query id (siz digit number).
In orer to obtain your personal token to identify with IBKR, go to Settings -> Account Settings -> Reporting (click on the settings symbol under Flex Query Services), and there you can generate your token or copy your token (long numeric code).

