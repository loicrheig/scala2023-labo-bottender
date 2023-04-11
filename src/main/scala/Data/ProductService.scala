package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  // TODO - Part 2 Step 2
  def getPrice(product: ProductName, brand: String): Double =
    product match
      case "biere" => 
        brand match
          case "Boxer" => 1.0
          case "Farmer" => 1.0
          case "Wittekop" => 2.0
          case "PunkIPA" => 3.0
          case "Jackhammer" => 3.0
          case "Tenebreuse" => 4.0
      case "croissant" => 2.0
        brand match
          case "Maison" => 2.0
          case "Cailler" => 2.0
  def getDefaultBrand(product: ProductName): BrandName =
    product match
      case "biere" => "Boxer"
      case "croissant" => "Maison"
end ProductImpl
