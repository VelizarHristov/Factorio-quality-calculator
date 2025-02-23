# Factorio quality calculator

This is WIP with some features already working. This is my to-do list:

- Bugfix - if ingredient quality = target quality then the calculator outputs nonsens
- Pull down all recipes in the game, let the user choose from them
	- Handle the case where a recipe recycles into itself, then you don't get to make it again, so the quality probabilities are lower, also the speeds are higher
	- Clean the data - remove the recycling recipes (first, make sure that they're all possible to statically calculate)
	- Handle the case where some of the inputs are liquids, then recycling doesn't return them so you need more of them, but also they work with higher-quality recipes
	- Investigate whether icon size can be reduced by removing some of them (since the files are hardcoding some icons)
- Calculate machine speeds and speed modules
	- Allow specifying the number and quality of beacons
	- Allow specifying the quality of crafting & recycling machines
	- Allow specifying the base crafting speed of the crafting machine
- Calculate multi-step processes (e.g. if there's the recipe chain A -> B -> C, and someone wanted to calculate the number of normal-quality As needed to make a legendary C)
- Improved input validation; improved error handling with infinities, NaN, etc
- Make it look nicer with the SAP UI5 components e.g. the inputs
- Move the version number to a footer, in the right corner
- Replace pixels in CSS with something more universal

If there's a feature that you wish to see, please open it as an Issue and I will consider adding it if it's in scope. The scope is anything that helps the user build production of quality items, which isn't already well covered by YAFC.
