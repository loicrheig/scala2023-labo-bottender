package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  // TODO - Part 2 Step 2
  def getPrice(product: ProductName, brand: Option[String]): Double =
    product match
      case "biere" => 
        brand match
          case Some("Boxer") => 1.0
          case Some("Farmer") => 1.0
          case Some("Wittekop") => 2.0
          case Some("PunkIPA") => 3.0
          case Some("Jackhammer") => 3.0
          case Some("Tenebreuse") => 4.0
          case None => getPrice(product, getDefaultBrand(product))
      case "croissant" =>
        brand match
          case Some("Maison") => 2.0
          case Some("Cailler") => 2.0
          case None => getPrice(product, getDefaultBrand(product))
  def getDefaultBrand(product: ProductName): BrandName =
    product match
      case "biere" => "Boxer"
      case "croissant" => "Maison"
end ProductImpl
