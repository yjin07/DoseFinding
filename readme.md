# Dose-Exposure-Response Modeling for Optimal Dose Selection

## Background

Dose selection is one of the most challenging and critical decisions in clinical trials, particularly in the development of oncology drugs. Traditional approaches have largely focused on the Maximum Tolerated Dose (MTD) as the key criterion for dose selection. However, this strategy may not be optimal for modern targeted therapies, which often have wider therapeutic indices and may not require dosing up to the MTD to achieve efficacy.

In response to these challenges, the FDA released its [most recent guidance](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/optimizing-dosage-human-prescription-drugs-and-biological-products-treatment-oncologic-diseases) in August 2024, emphasizing the need to move beyond MTD-focused strategies. The FDA recommends a more comprehensive approach, focusing on collecting relevant data and designing trials to identify optimized dosages. This includes considerations such as Clinical Pharmacokinetics (PK), Pharmacodynamics (PD), and the design of trials to compare multiple dosages.

## Our Approach

Building on the FDA’s recommendations, our work focuses on developing a **Dose-Exposure-Response model** that leverages data from PK analysis to achieve what we term **Exposure Informed Dose Selection**. The goal is to better capture the complex relationships between dose, exposure, and clinical response, leading to more informed and effective dose selection in clinical trials.

### Methodology

Our methodology involves the following key steps:

1. **Dose-Exposure (DE) Model**: 
   - This model relates the administered dose to the drug exposure in the body. 
   - **Integration of Covariates**: To account for inter-individual variability, additional covariates are introduced into the DE model. These covariates help capture clinically meaningful differences in exposure and include factors such as:
     - Weight
     - Age
     - Sex
     - Race and Ethnicity
     - Organ impairment
     - Genetic factors

2. **Exposure-Response (ER) Model**:
   - This model links drug exposure to clinical response. Depending on the nature of the response data, the model can handle both continuous and binary outcomes.
   - **Exposure-Response Functions**: Several models are considered to capture the relationship between exposure and response, including:
     - **Linear and Linear in Log Models**
     - **Logistic Model**
     - **Quadratic Model**
     - **Exponential Model**
     - **Beta Model**
     - **Emax and Sigmoid Emax Models**

### Implementation

To facilitate the application of these models in clinical trials, we have developed an R Shiny tool. This tool allows researchers and clinicians to implement the Dose-Exposure-Response models effectively, providing real-time insights into optimal dose selection based on PK analysis data.

## How to Use the Repository

### File Structure
```
├───webapp/
│   ├───app.R
│   ├───global.R
│   ├───ui.R
│   ├───server.R
│   ├───renv/
│   └───R/
│       ├───scripts
│       └───utils/
├───data/
```


### Description

- **webapp/**: This directory contains the components of the R Shiny application that implements the Dose-Exposure-Response models.
  - **app.R**: The main entry point for the Shiny app. It combines the UI and server components to run the application.
  - **global.R**: Contains global packages and functions that are accessible across the Shiny app.
  - **ui.R**: Defines the user interface (UI) of the Shiny app, including layout, input elements, and output displays.
  - **server.R**: Contains the server logic that processes inputs, performs calculations, and generates outputs for the Shiny app.
  - **renv/**: This directory is used for version control of R packages with `renv`. It stores the necessary package versions to ensure that the Shiny app runs consistently across different environments. After cloning the repository, you can use the following commands to set up the R environment:
    1. Run `renv::activate()` to activate the `renv` environment.
    2. Run `renv::restore()` to install all the required packages in the versions specified by the `renv.lock` file.
  - **R/**: This directory contains R scripts and utility functions used by the Shiny app.
    - **scripts**: have functions that are used within the Shiny app. These functions handle specific tasks such as data manipulation, model fitting, and generating plots.
    - **utils/**: Contains the fundamental code used to fit various models. These scripts provide the core functionality necessary to perform Dose-Exposure-Response modeling, including statistical methods and algorithms.

- **data/**: This directory contains simulated datasets used for model fitting, validation, and demonstration purposes within the Shiny app. These datasets are designed to represent different scenarios that may be encountered in dose-finding studies.

### Running the Shiny App

To run the Shiny app locally:

1. Clone this repository to your local machine.
2. Navigate to the `webapp` directory in RStudio.
3. Run `renv::activate()` to activate the R environment.
4. Run `renv::restore()` to install all the required R packages.
5. Once the environment is set up, run `shiny::runApp()` to start the application.

The Shiny app will open in your web browser, where you can interact with the Dose-Exposure-Response models, explore the simulated dataset, and upload your own data for analysis.


## Contributing

We welcome contributions to improve the models and the Shiny application. Please fork the repository, make your changes, and submit a pull request.


## Acknowledgments
This work was conducted by the author (Yisen Jin) as a Biostatistics Dose Finding Intern at BeiGene, under the guidance of Chester Lin, Director of Biostatistics at BeiGene USA, Inc. Contributions were also made by Jun Wang, Rui (Terry) Liu, and Bo Wei, Biostatisticians at BeiGene.


