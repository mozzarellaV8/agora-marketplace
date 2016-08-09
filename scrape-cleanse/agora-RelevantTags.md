# Agora - Relevant Tags (HTML)

What follows below is some manual parsing of the html files from the Agora Marketplace harvest. This was done in preparation for using `rvest` to import the information into dataframes. The html pages are from the directory `p` within the daily listing directories from the harvest. 

### product title via page title: "title"

e.g. _\<title>1kg pure bk-MDMA/ METHYLONE- with FREE shipping\</title>_

### product title via header: "#single-product h1"

e.g. _\<div id="single-product" class="nofirstmargin">_
	    _\<h1>1kg pure bk-MDMA/ METHYLONE- with FREE shipping\</h1>_


### product description: "#single-product"

This one is problematic so far. There are no clean css selectors I can find, nor xml. But it might be possible to simply scrape the whole page, and then use regex later to select out the description. It falls between the patterns `alt=""/>` and `Brought` on each page. 

Possibly within \<p> tags too

e.g.     _\<div style="clear: both;">\</div>_
    
_\<img src="/liabilities/p/ALDcahdCtn.jpg" style="display: block; float: right; padding: 0 0 20px 20px; max-width: 400px;" alt=""/>_
    
_1kg of pure bk-MDMA/ METHYLONE shipped worldwide using discreet methods.\<br/>\<br/>For more info please see our profile.\<br/>\<br/>Please note:\<br/>\<br/>Listing is for powder product- same quality as crystal but different texture + much cheaper, due to its appearance we must assure that quality or potency therefore is not affected. Some buyers prefer powder material.\<br/>We also offer translucent white crystal appearance- please see our listings.\<br/>\<br/>Shipping to EU,US not a problem. Tracking provided.\<br/>\<br/>Depending on receiving destination In unlikely event of getting items seized, lost or misplaced, we offer 50% re-shipping option.\<br/>\<br/>Buyers from high risk countries Australia, New Zealand, Russia, Ukraine, Germany, Scandinavia etc. order at own risk + must FE there are no refunds, no reshipping- please read our profile for info.\<br/>Brought to you by:_

### vendor name: "a.gen-user-link" 

e.g. _Brought to you by:\<br/>\<a class="gen-user-link" href="/vendor/drzheng"/>drzheng\</a>\<span class="gen-user-ratings"> [0 deals]\</span>_

### vendor rating: ".gen-user-ratings"

e.g. _Brought to you by:\<br/>\<a class="gen-user-link" href="/vendor/drzheng"/>drzheng\</a>\<span class="gen-user-ratings"> [0 deals]\</span>_

### ship_from: ".product-page-ships"

e.g. _\<div class="product-page-ships">_

### ship_to: div class=".product-page-ships"

e.g. _\<div class="product-page-ships">_

### list price: ".product-page-price"

e.g. _\<div class="product-page-price">0.00266844 BTC\</div>_

### feedback: ".embedded-feedback-list"

potentially also within \<h4> header.

h4 style="margin: 10px 0 5px 0;"
