# Agora - Relevant Tags - 'vendor' directory

What follows below is some manual parsing of the html files from the Agora Marketplace harvest. This was done in preparation for using `rvest` to import the information into dataframes. The html pages are from the directory `vendor` within the daily listing directories from the harvest. 

### vendor bio: ".vendorbio-description"

e.g. _\<div class="vendorbio-description">_

### vendor pgp public key: ".pgptoken"

e.g. _\<span class="pgptoken">-----BEGIN PGP PUBLIC KEY BLOCK-----\<br/>_


### feedback: ".embedded-feedback-list"

e.g. _\<div class="embedded-feedback-list">_

### product list: "#product-list" 

e.g. _\<div id="product-list">_

This div id refers to a table that contains: product name (link), product description preview, price in BTC, ship from location, ship to location. The following tags are for info to extract from each row. 

## children of the #product-list id:

### Product Name: "a"

raw: _\<td class="column-name">\<a href="/p/kiXPY0dXUV">Lorazepam (Ativan) 1 mg x 50 (free shipping)\</a>_

### Product Description preview: ".description-preview"

raw: _\<span class="description-preview">Top-quality branded generic equivalent of Ativan. 100% correct dosage guaranteed.\</span>_

### Price in BTC: "td"

e.g. _\<td>0.05989857 BTC\</td>

### Ship_From location: "td" or ".column-name~ td+ td"

Unfortunately shipping information doesn't seem to have it's own class or id - just the td tag. 

### Ship\_To location: "td" or ".column-name~ td+ td"

Unfortunately shipping information doesn't seem to have it's own class or id - just the td tag. 