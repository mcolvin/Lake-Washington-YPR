---
title: "Methods"
---

# Methods
## Study Area

The research took place on Lake Washington, in Washington County, Mississippi.
The lake is located 25 miles south of Greenville, along the Mississippi River.
An oxbow lake of the Mississippi River, Lake Washington is now one of the largest natural lakes in the state at 5,000
 acres of surface area.
The lake varies in depth ranging from 1.8 feet to 6.7 meters, with an average around 1.8 meters.
The lake consists of natural cover and structure, as well as, man-made cover.
The natural cover found in the lake includes cypress trees and aquatic vegetation.
The old river channels and ledges found throughout the lake make up the natural structure.
Man-made cover includes all of the piers, docks, rip-rap, and any sunken attractants found in the lake.
Lake Washington is well known for its crappie fishing and is a popular fishing destination for  Sportfish found in
 the lake include largemouth bass (Micropterus salmoides), bluegill (Lepomis macrochirus), channel catfish (Ictalurus
 punctatus), Black Crappie (Pomoxis nigromaculatus), and White Crappie (Pomoxis annularis).
Black and White Crappie are often fished for as one species in this system, and in turn are managed for as one species.
Currently crappie harvest is regulated with a 254 mm MLL and a daily bag limit of 30 fish.
This system is unique because regulations allow anglers to keep 5 fish under the 254 mm MLL, locally known as the “5 under rule”.

## Data Collection

Fish sampling was conducted on October 4, 2015 – October 6, 2015.
Trap and lead nets were set on October 4 in twelve different locations in Lake Washington.
The traps were then checked and reset on October 5, and checked again on October 6.
At each net location, water temperature and depth was recorded, as well as any additional comments.
Every fish that was caught in a net was identified and total length was recorded to the nearest
 millimeter (mm).
The first 5 crappie caught from each centimeter bin were kept to be weighed and aged at a later date. 

The trap nets are constructed of two 3 foot x 6 foot x 1 inch fiberglass frames, with four 2.5 foot
 diameter hoops in the fiberglass.
Hoops are 2 feet apart.
The netting is square knotless treated nylon with 0.5 inch openings.
The trap nets are 40 feet in length, 3 feet in depth, and attached to the first frame.
The lead nets that we used were constructed with two hoops facing each other, and are connected with
 a lead that attaches to each hoop net.
The net and lead are constructed from 1 inch treated bar mesh.
The hoops are constructed of 7 fiberglass hoops, with a diameter of 3.5 feet, which are separated
 by 2 feet with throats located at the 2nd and 4th hoop.
The lead between the two hoops nets is 30 feet in length.

## Mortality and Exploitation

Mortality rates were provided by Mississippi Department of Wildlife, Fisheries, and Parks, N. Aycock,
 unpublished data.
In 2012 MDWFP caught and aged crappie from Lake Washington in order to establish an annual
 mortality rate.
An annual mortality rate of 66% was calculated using a weighted catch curve.
A tag return study was done in 2014-2015 in order to establish an annual exploitation rate.
The study was done by tagging 400 fish greater than the MLL (>254 mm) and a reward was given out for
each returned tags. After taking into account tagging mortality, non-reporting, and tag loss,
 the annual exploitation rate was found to be 41%.
We would use this exploitation rate for our yield per recruit models.

## Length-weight and Age-length Relationship

The first 5 crappie that were caught, from each centimeter bin, were measured for total length and kept to be weighed. This fish were then measured again and weighed before being aged. The data was entered into excel and the parameters for the length-weight relationship were established. A scatterplot was then created in order to show the relationship.

	  	(1)

Weights were recorded and otoliths were pulled and taken to Mississippi State University to be aged.
Aging was done by counting growth rings on the otolith under a Leica DFC 290 HD microscope.
Each otolith was aged by Maddy Ruble and myself. The otoliths were ages independently and then compared.
If a disagreement occurred, the otolith was reviewed together until a consensus was reached.
In order to establish a growth curve, needed to run Yield per Recruit models, we fit a von Bertalanffy
 growth function to the size and age of the crappie aged from Lake Washington.

$L\left( t \right)={{L}_{\infty }}(1-{{e}^{k~\cdot (t-{{t}_{0}})~}})$  (2)
        
## Yield Per recruit Models

Traditionally YPR models only account for yield occurring above the MLL.
Since Lake Washington has the unique regulation allowing for harvest of 5 crappie under the MLL,
 we had to develop a modified model.
The new, modified model needs the same information as a traditional Beverton-Holt model: weight-length
 relationship, length-age relationship, and a mortality rate.
By using this new model we were able to evaluate the 3 proposed MLL, 254 mm (current), 279.4 mm, and
 304.80; while accounting for various fishing mortalities below the MLL (0, 0.01, 0.05, 0.1, 0.2).
By evaluating various fishing mortality below the MLL we were able to evaluate the potential effect of
 fishing mortality below the MLL, while still evaluating that effect that different length limits would
 have on yield in the system.
This would also allow us to see when, if at all, growth overfishing could occur in the system.

 $\frac{dN}{dt}=-({{N}_{t}}\cdot ({{F}_{t}}+M))$	 	(3)

 	

$\frac{dY}{dt}={{F}_{t}}\cdot {{N}_{t}}\cdot {{W}_{t}}$	  	(xx)

${{W}_{t}}=a\cdot {{\left( {{L}_{\infty }}\cdot (1-{{e}^{k\cdot (t-{{t}_{0}})}} \right)}^{b}}$	  					

 $F(t)=\left\{ \begin{matrix}
   {{F}_{below\,mll}}, & {{t}_{h}}\le t<{{t}_{r}}  \\
   {{F}_{above\,mll}}, & {{t}_{r}}\le t  \\
\end{matrix} \right.$
	  	(4) 
	  
$\frac{dEggs}{dt}={{N}_{t}}\cdot [Sex\,ratio]\cdot {{P}_{mature,t}}\cdot Fecundit{{y}_{t}}$
${{P}_{mature}}=\frac{1}{1+{{e}^{(-(t-{{t}_{mat}})/{{\sigma }_{mat}})}}}$ 
•	t=1 to lambda using adams bashford
•	R = 1000 
•	Implemented in R with a shiny interface 
