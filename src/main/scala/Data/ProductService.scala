package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  // TODO - Part 2 Step 2
  def getPrice(product: ProductName, brand: BrandName): Double =
    product match
      case "biere" => 
        brand match
          case "boxer" => 1.0
          case "farmer" => 1.0
          case "wittekop" => 2.0
          case "punkIPA" => 3.0
          case "jackhammer" => 3.0
          case "tenebreuse" => 4.0
          case "" => getPrice(product, getDefaultBrand(product))
      case "croissant" =>
        brand match
          case "maison" => 2.0
          case "cailler" => 2.0
          case "" => getPrice(product, getDefaultBrand(product))
  def getDefaultBrand(product: ProductName): BrandName =
    product match
      case "biere" => "Boxer"
      case "croissant" => "Maison"
end ProductImpl
