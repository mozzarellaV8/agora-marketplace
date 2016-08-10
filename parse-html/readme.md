# Agora Marketplace Analysis - parse and cleanse

What follows here are detailed notes and images on the parsing/cleansing process to get the raw html data into R. 

- [agDirectoryNotes.md](agDirectoryNotes.md) - initial looks at the harvested data and extraction process
- [ag-RelevantTags-p.md](ag-RelevantTags-p.md) - relevant tags from html - 'p' (individual product) directory
- [ag-RelevantTags-vendor.md](ag-RelevantTags-vendor.md) - relevant tags from html - 'vendor' directory

# Relevant Tags

For extraction: 

**'p' directory:**

- product title via page title: **"title"**
- product title via header: **"#single-product h1"**
- product description: **"#single-product"**
- vendor name: **"a.gen-user-link"**
- vendor rating: **".gen-user-ratings"**
- ship_from: **".product-page-ships"**
- ship_to: div class= **".product-page-ships"**
- list price: **".product-page-price"**
- vendor feedback: **".embedded-feedback-list"**

**'vendor' directory:**

- vendor bio: **".vendorbio-description"**
- vendor pgp public key: **".pgptoken"**
- feedback: **".embedded-feedback-list"**
- product list (as a table): **"table.products-list"**
- product list : **"#product-list"**

_children of div id Product List:_

- Product Name: **"#product-list a"**
- Product Description preview: **".description-preview"**
- Price in BTC: **"#product-list td"**
- Ship_From location: **".column-name~ td+ td"**
- Ship\_To location: **"td"** or **".column-name~ td+ td"**

**'cat' directory:**

- Category on Page- **".topnav-element"**
- Subcategory List - **".leftmenu-subelements a"**
- Main Categories - **".leftmenu-element a"**
- Category Product List Table - **"table.products-list"**
- Listing Headers - **".products-list-header"**
- Product Name - **".column-name a"**
- Product Description (preview) - **".description-preview"**
- Price - **".products-list td"**
- Shipping info - **".column-name~ td+ td"** **"//td[((count(preceding-sibling::*) + 1) = 4)]"** (xpath)
- Vendor - **"a.gen-user-link"**
- Vendor Rating - **".gen-user-ratings"**

