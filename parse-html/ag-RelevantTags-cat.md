# Agora - Relevant Tags - 'cat' directory

What follows below is some manual parsing of the html files from the Agora Marketplace harvest using a combination of Chrome Developer Tools and selectorgadget. This was done in preparation for using `rvest` to import the information into dataframes. 

The html pages are from the directory `cat` within the daily listing directories from the harvest. The `cat` folder for each crawl contains pages sorted by category.

Tags for other folders:
- [p directory tags](ag-RelevantTags-p.md)
- [vendor directory tags](g-RelevantTags-vendor.md)
- [initial directory notes](agDirectoryNotes.md)


## 'cat' tags:

- Category on Page- ".topnav-element"
- Subcategory List - ".leftmenu-subelements a"
- Main Categories - ".leftmenu-element a"
- Category Product List Table - "table.products-list"
- Listing Headers - ".products-list-header"
- Product Name - ".column-name a"
- Product Description (preview) - ".description-prevew"
- Price - ".products-list td"
- Shipping info - ".column-name~ td+ td"; "//td[((count(preceding-sibling::*) + 1) = 4)]" (xpath)
- Vendor - "a.gen-user-link"
- Vendor Rating - ".gen-user-ratings"
