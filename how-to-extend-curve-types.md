## How to add new curve type

The architecture is based on the dependency inversion principle. 

![Curve types hierarchy](assets/images/PointsSetsHierarchy.png)

![Diagram](assets/images/ExtendingPointsSets.png)

## Step-by-step instructions

1. Create new module and define new curver class and descent it from TNamedPointsSet. Implement virtual methods.

1. Add instantiation of TCurveTypesSingleton and call of registration function to the module.

1. Add new module to the project. New class should be automatically displayed in the main menu.

{% include google_ads.html %}
