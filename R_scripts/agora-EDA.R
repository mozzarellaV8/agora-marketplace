# Agora Marketplace
# Exploratory Vis

# explore -------------------------------------------------

# subset the top vendors
topvendors <- subset(vendors, vendors$NumListings > 25000)

vendorplot01 <- ggplot(topvendors, aes(x = reorder(Vendor, NumListings),
                                       y = NumListings, color = NumListings)) +
  geom_point(size = 6) + 
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora Marketplace: Top Vendors by Number of Listings",
       x = "", y = "number of listings")

vendorplot01

locationplot01 <- ggplot(location, aes(x = reorder(Location, NumListings),
                                       y = NumListings, colour = NumListings)) +
  geom_point(size = 4) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora Marketplace: Locations Shipped From",
       x = "", y = "number of listings")

locationplot01

# subset locations to number of listings over 100
toplocations <- subset(location, location$NumListings > 100)

locationplot02 <- ggplot(toplocations, aes(x = reorder(Location, NumListings),
                                           y = NumListings, color = NumListings)) +
  geom_point(size = 6) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora Marketplace: Locations Shipped From",
       x = "", y = "number of listings")

locationplot02

locationplot03 <- ggplot(toplocations, aes(x = NumListings, y = Location, 
                                           color = NumListings)) +
  geom_point(size = 6) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(axis.text.x = element_text(angle = 0)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora Marketplace: Locations Shipped From",
       x = "# of listings", y = "location shipped from") 

locationplot03
