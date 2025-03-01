# Factorio quality calculator

This is WIP with some features already working.

To do - main use cases and major improvements:
- Improve handling of recipes, handle cases where:
	!! Idea: make use of the number of times that each machine was used (including recycling) !!
	- Some of the inputs are fluids (IDEA: keep track of how many inputs come from recycling; then compensate for the fluids by demanding them from the inputs)
	- There are catalysts
		* Brainstorming in text:
			- If there is only one output, then we can just use the regular function, but multiplying the productivity by a lower number (if 2/3 of the outputs are catalysts then 1/3 aren't; applying the entire productivity bonus to those is the same as applying 1/3 of the productivity bonus to the entire output)
			- If the main product is not a catalyst, but a byproduct is, then do A
			- If a byproduct is a catalyst, but the main product isn't, then do A
			- If both the main product and a byproduct are catalysts to a different extent, then do A
			- A: First calculate the productivity on the main product - if it's partly or entirely a catalyst that decreases its productivity (see first point for the formula); then do the entire calculation with that number (including recycling, or skipping it); finally, count the number of machine uses, and multiply how much of each byproduct they would make, while also separately for each byproduct taking into account the catalyst degree and productivity.
	- 
	- ! Reconsider all solutions below, if they can be simpler after the solutions above !
	- 
	- There are multiple outputs, and they don't recycle into the inputs (maybe reuse the below, idk)
	- The output recycles into something other than either itself or the inputs (solution: first set target quality = ingredient quality, then upscale until we get enough of the target quality, so we get a lot of byproducts of lower quality)
	- Some output byproducts are liquids (easy solution: just add them as byproducts - make sure to handle recycling correctly)
	- A recipe makes from A to B, then B recycles into itself (easy solution -  first set target quality = ingredient quality, then combine results with recycling the lower-than-target-quality products into themselves)
	- All inputs are liquids (easy solution - prevent ingredient quality>1, then reuse solution which recycles into itself)
	- All outputs are liquids (easy solution - target quality set to 1; treat all output qualities as 1)
	- The user has selected a recycling recipe (easy solution - fix 0% productivity and 0% quality for the fake "machine", with a recipe that is 1->1)
	- After the above are implemented, add a radio button for the recycling mode - no recycling or recycling. Is there a third option? If no then make it a checkbox instead.
- Easier input for machine & module setup
	- Allow specifying the quality of crafting & recycling machines
	- Allow adding modules and calculate their effects
	- Support beacons and speed modules
- Calculate multi-step processes (e.g. if there's the recipe chain A -> B -> C, and someone wanted to calculate the number of normal-quality As needed to make a legendary C)
- After multi-step processes are supported - add a button to auto add any processes for which there is only one possible recipe (for example if speed module 2 was amongst the inputs); maybe do it recursively, until there is nothing else to add; maybe add a modal which lets the user easily keep or remove some of the results.
- Allow users to save their progress in cookies (auto save it actually); keep multiple separate factories, so that the user can start freshly but later return to what they previously did.
- Add icons for:
	- Recipe
	- Ingredients
	- Outputs
	- Machines
	- Recyclers
	- (Anything else?)

To do - minor improvements:
- Improved input validation; improved error handling with infinities, NaN, etc
- Make it look nicer with the SAP UI5 components e.g. the inputs
- Move the version number to a footer, in the right corner
- Investigate whether icon size can be reduced by removing some of them (since the JSON is referencing any icons that are used)
- Remove the recycling recipes (first, make sure that they're all possible to dynamically calculate) (keep any that don't match the dynamic calculation)

Bugs:
- When unlocked quality = target quality > 1, for the last quality, the listed machine speed is used to calculate the number of machines needed, but what should be used instead is the machine with either prod modules if possible, or speed modules otherwise. Maybe add an (i) icon which on hover informs the user that they can use speed modules / beacons there - only show it when more than 1.00 legendary machines are necessary.
	- What if the user wants to use quality modules, when prod are also an option? Then they will want to use max prod modules on the last step.
- We calculate recycler speed incorrectly - in vanilla it's weird - if it takes 10 seconds to take ten concrete, then it takes 10/8 seconds to recycle ONE concrete instead of TEN.
- Replace pixels in CSS with something more universal

Low priority:
- Enable mining recipes - display how many raw resources will be drained (allow quality & modules on drills, as well as mining productivity research)

If there's a feature that you wish to see, please open it as an Issue and I will consider adding it if it's in scope. The scope is anything that helps the user build production of quality items, which isn't already well covered by YAFC.
