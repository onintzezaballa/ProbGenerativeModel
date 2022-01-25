# A Probabilistic Generative Model for Learning the Progression Patterns of Treatments

[![Made-with-R](https://img.shields.io/badge/Made%20with-R-blue)](/AMRC_Python) [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](#support-and-author)

This repository is the official implementation of .........

The proposed AMRCs account for multivariate and higher order time changes, provide performance guarantees at specific time instants, and efficiently update classification rules. 

AMRCs provide instantaneous bounds for probabilities of error and bounds for accumulated mistakes.

## Source code

[![Made-with-R](https://img.shields.io/badge/Made%20with-R-blue)](/AMRC_Python)

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


## Installation and evaluation

To train and evaluate the model in the paper, run this command for Python:

```console
cd AMRC\AMRC_Python
python run_AMRC.py

```


## Data

We use a synthetic dataset and ......

### Synthetic data


### Real-world datasets

We save the data in .mat files that contain two arrays: X composed by instances and Y composed by labels such as [`usenet2.mat`](Imvt.mat).

## Test case

We display in this reposity an example for the "Usene2" dataset. 

We run the Python code

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


# Support and Author
Onintze Zaballa Larumbe
onintzezaballa@gmail.com
