# agora - directory + html page structure notes

What follows below are notes on the structure of the harvest. Handwritten notes and images were a way to deal with the unstructured nature and scale of the data. Data was originally compiled by indpendent researcher Gwern and is available [here](gwern.net/Black-market archives).


![AgDirectoryNotes.jpg](AgDirectoryNotes.jpg)


The main directory, with weekly and sometimes more frequent crawls. 208 total.

![agScrape-mainDir.png](agScrape-mainDir.png)


The `cat` directory, which contains pages grouped by category. These categories are found on the left sidebar of the webpages. Examples of categories include `Chemicals`, `Counterfeits`, `Data`, and `Drugs`. These categories drill down into more specific categories such as `Weed`, `MDMA`, `Books`, and such.

![agScrape-catDir.png](agScrape-catDir.png)

The `p` directory, which contains individual product listings. This will likely be the main directory working data will be harvested from. From the pages in `p`, we have information on 

	- name of product
	- description
	- list price
	- vendor name
	- ship from
	- ship to
	- vendor feedback

![agScrape-pDir.png](agScrape-pDir.png)

The `vendor` directory, which acts as a vendor's 'storefront' with a description of their wares and process - and also a list of all their listings for that particular day.

![agScrape-vendorDir.png](agScrape-vendorDir.png)







