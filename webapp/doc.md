# R Shiny Web Application: Dose-Exposure-Response Modeling

## Overview

This R Shiny web application provides an interactive platform for implementing and exploring Dose-Exposure-Response models, which are designed to optimize dose selection in clinical trials, particularly in oncology. The application allows users to input data, visualize results, and perform model-based dose selection using both simulated and user-provided datasets.

## Models Implemented

### 1. **Dose-Exposure (DE) Model**
The Dose-Exposure (DE) model is the first step in the two-step modeling framework. This model establishes the relationship between the administered dose and the drug exposure in the body. The primary goal is to account for the variability in drug exposure across individuals receiving the same dose, which is critical for optimizing dose selection.

- **Power Model**: The DE model typically uses a power model to describe the relationship between dose and exposure:

  $$\log(C) = \beta_0 + \beta_1 \log(d) + \epsilon_C$$

  where:
  - $C$ denotes the exposure level.
  - $d$ denotes the administered dose.
  - $\epsilon_C$ represents the residual variability in exposure.

- **Integration of Covariates**: The DE model can incorporate various covariates to capture clinically meaningful differences in exposure, including:
  - Weight
  - Age
  - Sex
  - Race and Ethnicity
  - Organ impairment
  - Genetic factors

### 2. **Exposure-Response (ER) Model**
The second step in the framework is the Exposure-Response (ER) model, which links drug exposure to clinical response. This model is crucial for understanding how variations in drug exposure impact treatment outcomes.

- **Continuous and Binary Response Models**: The ER model can handle both continuous and binary outcomes:
  - **Continuous Case**: Assumes the response is normally distributed around a mean response $\mu_c$.
  - **Binary Case**: Uses a logistic regression model to estimate the probability of a positive response.

- **Exposure-Response Functions**: Several functional forms are implemented to capture the relationship between exposure and response, including:
  - **Linear and Linear in Log Models**: Simple linear relationships and log-linear relationships. 
      $$f(c, \theta) = E_0 + \delta c  \quad\quad\text{(Linear)}$$
      $$f(c, \theta) = E_0 + \delta\log(c + off) \quad\quad\text{(Linear in Log)}$$
  - **Logistic Model**: A sigmoid function capturing monotone, sigmoid-shaped exposure-response relationships.
      $$f(c, \theta) = E_0 + E_{\max} / \{1 + \exp[(EC_{50} - c) / \delta]\}$$
  - **Quadratic Model**: A model allowing for potential non-monotonic relationships.
      $$f(c, \theta) = E_0 + \beta_1 c + \beta_2 c^2$$
  - **Exponential Model**: Captures sub-linear or convex shapes in the exposure-response relationship.
      $$f(c, \theta) = E_0 + E_1 (\exp(\frac{c}{\delta}) - 1)$$
  - **Beta Model**: A flexible model that can describe complex, non-monotonic relationships.
      $$f(c, \theta) = E_0 + E_{\max}B(\delta_1, \delta_2)(c/scal)^{\delta_1}(1-c/scal)^{\delta_2}$$ 
      where $B(\delta_1,\delta_2) = (\delta_1 + \delta_2)^{\delta_1+\delta_2}/(\delta_1^{\delta_1}\delta_2^{\delta_2})$
  - **Emax and Sigmoid Emax Models**: Models that describe saturation effects, where the response increases with exposure but eventually plateaus.
      $$f(c, \theta) = E_0 + E_{\max}\frac{c}{EC_{50} + c} \quad\quad\text{(Emax)}$$
      $$f(c, \theta) = E_0 + E_{\max}\frac{c^h}{EC_{50}^h + c^h} \quad\quad\text{(Sigmoid Emax)}$$

## Application Features

### 1. **Data Input**
Users can upload their own datasets or use the preloaded simulated datasets provided within the application. The input data should include variables corresponding to dose levels, exposure measurements, and response outcomes. Additional covariates can be included to account for patient-specific factors that could be included in the DE **model**.

### 2. **Model Fitting**
The application fits the Dose-Exposure and Exposure-Response models to the provided data. Users can select the specific model form they wish to apply and configure relevant parameters.

### 3. **Visualization**
The application provides visualizations of the fitted models, including:
- Dose vs. Exposure plots
- Exposure vs. Response plots
- Residual diagnostics to assess model fit

### 4. **Model Evaluation**
Users can evaluate model performance based on various metrics, such as goodness-of-fit statistics and visual diagnostics. This helps in selecting the most appropriate model for the data at hand.

### 5. **Dose Optimization**
The application allows users to perform dose optimization based on the fitted models. This includes estimating the optimal dose that maximizes efficacy while minimizing toxicity, using the dose-exposure-response framework.

## How to Use the Web Application

1. **Access the Application**: Run the Shiny app either locally by cloning the repository and following the setup instructions or by accessing the hosted version (if available).
2. **Upload Data**: Navigate to the data input section to upload your dataset. Ensure the data format matches the required structure.
3. **Select Models**: Choose the Dose-Exposure and Exposure-Response models you wish to apply to your data.
4. **Visualize Results**: Use the visualization tools to explore the fitted models and understand the relationship between dose, exposure, and response.
5. **Optimize Dose**: Utilize the dose optimization tools to determine the best dose levels for your study.

## File Structure

