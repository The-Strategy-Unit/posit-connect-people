# posit-connect-people

[<img src='https://img.shields.io/badge/Posit_Connect-deployed-447099?style=flat&labelColor=white&logo=Posit&logoColor=447099'>](https://connect.strategyunitwm.nhs.uk/posit-connect-people/)

## About

A simple interface for a quick understanding of users and content on our Posit Connect server.
Overcomes some missing functionality in the platform itself.

The report itself is [deployed to Connect](https://connect.strategyunitwm.nhs.uk/posit-connect-people/) on schedule (login and permissions required).

## For developers

### Local render

To run locally, you'll need to add an `.Renviron` file to the project root, which contains the keys specified in the provided `.Renviron.example` file.
Contact the Data Science team for the required values.

### Deploy to Connect

To redeploy to Posit Connect after changes, run the `deploy.R` script.

### Possible API changes

It's possible that the Connect API will change slightly from time-to-time.
That could cause certain columns to be incorrect, or at worst, prevent the table from rendering completely
If in doubt, check the metadata on Connect itself.
