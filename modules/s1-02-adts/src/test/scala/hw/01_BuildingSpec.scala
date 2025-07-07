package hw

class BuildingSpec extends munit.FunSuite:
<<<<<<< HEAD
  test("countOldManFloors should return the number of men older than the specified age"):
    assertEquals(1, 1) // Implement yourself

  test("countOldManFloors should return 0 if there are no men older than the specified age"):
    assertEquals(0, 0) // Implement yourself

  test("countOldManFloors should return 0 if there are no men at all in the building"):
    assertEquals(0, 0) // Implement yourself

  test("womanMaxAge should find age of the oldest woman in the building"):
    assertEquals(Some(100), Some(100)) // Implement yourself

  test("womanMaxAge should return None if there are no women in the building"):
    assertEquals(None, None) // Implement yourself

  test("countCommercial should return number of commercial establishments in the building"):
    assertEquals(4, 4) // Implement yourself

  test("countCommercial should return 0 if there are no commercial establishments in the building"):
    assertEquals(3.5, 3.5) // Implement yourself
=======

  val oldMan = Resident(70, Gender.Male)
  val youngMan = Resident(25, Gender.Male)
  val woman1 = Resident(40, Gender.Female)
  val woman2 = Resident(100, Gender.Female)
  val middleMan = Resident(50, Gender.Male)

  val shop = Building.Business("Магазин")
  val cafe = Building.Business("Кафе")
  val bar = Building.Business("Бар")
  val office = Building.Business("Офис")

  val building1 = Building(
    "Ленина 10",
    Building.Floor.ResidentialFloor(
      youngMan, woman1, Some(
        Building.Floor.ResidentialFloor(
          oldMan, woman2, Some(
            Building.Floor.Commercial(List(shop, cafe), Some(
              Building.Floor.Attic(Building.AtticType.Commercial(bar))
            ))
          )
        )
      )
    )
  )

  val building2 = Building(
    "Без стариков",
    Building.Floor.ResidentialFloor(
      youngMan, woman1, Some(
        Building.Floor.Commercial(List(shop), None)
      )
    )
  )

  val building3 = Building(
    "Женский дом",
    Building.Floor.ResidentialFloor(
      woman1, woman2, Some(
        Building.Floor.Attic(Building.AtticType.Empty)
      )
    )
  )

  val building4 = Building(
    "Торговый центр",
    Building.Floor.Commercial(List(shop, cafe, office), Some(
      Building.Floor.Attic(Building.AtticType.Commercial(bar))
    ))
  )

  val emptyBuilding = Building(
    "Пустой дом",
    Building.Floor.Attic(Building.AtticType.Empty)
  )

  test("countOldManFloors should return the number of floors with old men") {
    assertEquals(Building.countOldManFloors(building1, 60), 1)
  }

  test("countOldManFloors should return 0 if there are no men older than the specified age") {
    assertEquals(Building.countOldManFloors(building2, 60), 0)
  }

  test("countOldManFloors should return 0 if there are no men at all in the building") {
    assertEquals(Building.countOldManFloors(building3, 60), 0)
  }

  test("womanMaxAge should find the age of the oldest woman in the building") {
    assertEquals(Building.womanMaxAge(building1), Some(100))
  }

  test("womanMaxAge should return None if there are no women in the building") {
    assertEquals(Building.womanMaxAge(building4), None)
  }

  test("countCommercial should return number of commercial establishments in the building") {
    assertEquals(Building.countCommercial(building1), 3)
  }

  test("countCommercial should return 0 if there are no commercial establishments in the building") {
    assertEquals(Building.countCommercial(building3), 0)
  }

  test("countCommercialAvg should return the average number of commercial establishments") {
    val buildings = List(building1, building4)
    assertEquals(countCommercialAvg(buildings), 3.5)
  }

  test("countCommercialAvg should return 0.0 if no buildings have commercial establishments") {
    val buildings = List(building3, emptyBuilding)
    assertEquals(countCommercialAvg(buildings), 0.0)
  }
>>>>>>> solutions
