# Adaptive Minimax Classification by Tightly Tracking Underlying Distributions (AMRC)

[![made-with-python](https://img.shields.io/badge/Made%20with-Python-1f425f.svg)](/AMRC_Python) [![Made with!](https://img.shields.io/badge/Made%20with-MATLAB-red)](/AMRC_Matlab)  [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](#support-and-author)

This repository is the official implementation of Adaptive Minimax Classification by Tightly Tracking Underlying Distributions.

The proposed AMRCs account for multivariate and higher order time changes, provide performance guarantees at specific time instants, and efficiently update classification rules. 

<img src="instantaneous_bounds.gif" width="500" height="500"/>
<img src="mistake_bounds.gif" width="500" height="500"/>

AMRCs provide instantaneous bounds for probabilities of error and bounds for accumulated mistakes.

## Source code

[![made-with-python](https://img.shields.io/badge/Made%20with-Python-1f425f.svg)](AMRC_Python) 
[![Made with!](https://img.shields.io/badge/Made%20with-MATLAB-red)](AMRC_Matlab)

AMRC folder contains the Python and Matlab foldes that includes the Python and Matlab scripts, respectively, required to run the code.

### Python code

[AMRC_ Python](/AMRC_Python)  folder contains Python scripts required to execute the method:

* run_AMRC.py is the main file. In such file we can modify the values of hyper-parameters such as  <img src="https://render.githubusercontent.com/render/math?math=\lambda_0">, the order, and the feature mapping. In addition, such function tracks uncertainty sets, provides performance guarantees at specific times, and updates classifier parameters.
* efficient_learning.py efficiently updates classifier parameters.
* prediction.py assigns label to instances.
* feture_mapping_function.py calculates feature vectors using linear or random Fourier features (RFF) feature mappings.
* tracking_uncertainty_sets.py contains functions that obtain mean vector estimates and confidence vectors,  update variances of random noises, and initialize mean vector estimates, confidence vectors, and defines matrices and vectors that are used to update mean vector estimates and confidence vectors.
* requeriments.txt contains versions of Python modules


#### Requirements

The requirements are detailed in the requeriments.txt file. Run the following command to install the requeriments:

```setup
cd AMRC\AMRC_Python
pip install -r requirements.txt
```

### Matlab code

[AMRC_Matlab](/AMRC_Matlab) folder contains Matlab scripts required to execute the method:

* run_AMRC.m is the main file. In such file we can modify the values of hyper-parameters such as <img src="https://render.githubusercontent.com/render/math?math=\lambda_0">, the order, and the feature mapping.
* AMRC.m tracks uncertainty sets, provides performance guarantees at specific times, and updates classifier parameters.
* initialize_tau.m initializes mean vector estimates, confidence vectors, and defines matrices and vectors that are used to update mean vector estimates and confidence vectors.
* tracking.m obtains mean vector estimates and confidence vectors.
* feature_vector.m calculates feature vectors using linear or random Fourier features (RFF) feature mappings.
* learning.m efficiently updates classifier parameters.
* predict_label.m assigns label to instances.

## Installation and evaluation

To train and evaluate the model in the paper, run this command for Python:

```console
cd AMRC\AMRC_Python
python run_AMRC.py

```

and for Matlab:

```console
cd AMRC\AMRC_Matlab
matlab run_AMRC.m
```

## Data

We use a synthetic dataset and 13 publicly available datasets.

### Synthetic data


<img src="synthetic_data.gif" width="500" height="500"/>

| Dataset | Time Steps | Dimensionality of instances | Number of labels | Majority class | Moving average of one |
|:---------|------------:|---------------------------:|-----------------:|----------------:|------------------------:|
| Weather | 18,159 | 8 | 2 | 68.6 | 32.0  |
| Elec | 1,148 | 4 | 2 | 59.1 | 52.4 |
| Airlines | 539,383 | 7 | 2 | 55.5 | 41.9  |
| German | 1,000 | 24 | 2 | 70.0 | 43.0|
| Chess  | 503| 8 | 2 | 60.6 | 43.4  |
| Phishing | 11,055 | 46 | 2 | 86.0 | 21.5 |
| Usenet 1 | 1,500 | 99 | 2 | 53.3 | 67.0 |
| Usenet 2 | 1,500 | 99 | 2 | 66.7 |  66.6 |
| Email | 1,498 | 913 | 2 | 53.3 | 66.2 |
| C. card | 284,806 | 30 | 2 | 99.8 | 0.33  |
| S. Grid | 60,000 | 13 | 2 | 63.8 | 46.2 |
| Shuttle | 100,001 | 9 | 4 | 35.2 | 64.9   |
| Poker | 829,201 | 10 | 10 | 50.1 | 25.5|

### Real-world datasets

We save the data in .mat files that contain two arrays: X composed by instances and Y composed by labels such as [`usenet2.mat`](Imvt.mat).

## Test case

We display in this reposity an example for the "Usene2" dataset. 

We run the Matlab code

```train
cd AMRC\AMRC_Matlab
matlab run_AMRC.m
```
or Python code

```train
cd AMRC\AMRC_Python
python run_AMRC.py 
```
 and they output:
 
```train
AMRC for order 1 has a mistakes rate 22.5% in "Usenet2" dataset using linear feature mapping
```

We can choose a different dataset and modify parameters values ( <img src="https://render.githubusercontent.com/render/math?math=\lambda_0">, order, and feature mapping) in the main files, that is in run_AMRC.m for Matlab and run_AMRC.py for Python.

## Results

Our model achieves the following performance on :

| Dataset | Linear k = 0 | Linear k = 1 | Linear k = 2 | RFF k = 0 | RFF k = 1 | RFF k = 2 |
|:--------------------------|----------------------------------------:|----------------------------------------:|----------------------------------------:|-------------------------------------:|-------------------------------------:|-------------------------------------:|
| Weather                  | 32.4                                   | 32.5                                   | 32.5                                   | 32.3                                | 32.3                                | 32.3                                |
| Elec                     | 40.0                                   | 39.4                                   | 39.9                                   | 35.8                                | 35.8                                | 37.4                                |
| Airlines                 | 46.9                                   | 47.0                                   | 49.2                                   | 39.0                                | 38.9                                | 39.0                                |
| German                   | 30.4                                   | 30.7                                   | 30.8                                   | 30.1                                | 30.3                                | 30.9                                |
| Chess                    | 38.0                                   | 37.6                                   | 39.2                                   | 26.8                                | 27.7                                | 25.7                                |
| Phishing                 | 0.99                                   | 0.71                                   | 1.28                                   | 11.1                                | 11.0                                | 12.2                                |
| Usenet1                  | 32.6                                   | 32.3                                   | 34.0                                   | 36.2                                | 35.7                                | 35.8                                |
| Usenet2                  | 21.4                                   | 22.5                                   | 23.22                                  | 30.7                                | 30.9                                | 30.8                                |
| Email                    | 24.1                                   | 26.4                                   | 27.5                                   | 42.9                                | 43.7                                | 43.4                                |
| C. card                  | 0.33                                   | 0.30                                   | 0.31                                   | 0.18                                | 0.17                                | 0.17                                |
| S. grid                  | 36.1                                   | 36.0                                   | 36.1                                   | 35.9                                | 35.8                                | 36.0                                |
| Shuttle                  | 59.0                                   | 59.1                                   | 59.2                                   | 15.2                                | 15.2                                | 15.3                                |
| Poker                    | 26.2                                   | 25.5                                   | 26.0                                   | 22.3                                | 21.9                                | 22.6                                |
