test = {
  'name': 'Problem 6A',
  'partner': 'A',
  'points': 3,
  'suites': [
    {
      'cases': [
        {
          'code': r"""
          >>> # Testing NinjaAnt parameters
          >>> ninja = NinjaAnt()
          >>> ninja.armor
          d89cf7c79d5a479b0f636734143ed5e6
          # locked
          >>> NinjaAnt.food_cost
          62674984f877ec783f37e8b8b9c264d0
          # locked
          """,
          'hidden': False,
          'locked': True
        },
        {
          'code': r"""
          >>> # Testing NinjaAnts do not block bees
          >>> p0 = colony.places["tunnel_0_0"]
          >>> p1 = colony.places["tunnel_0_1"]
          >>> bee = Bee(2)
          >>> p1.add_insect(bee)
          >>> p1.add_insect(NinjaAnt())
          >>> bee.action(colony)  # shouldn't attack ant, move past it
          >>> bee.place is p0
          c7a88a0ffd3aef026b98eef6e7557da3
          # locked
          """,
          'hidden': False,
          'locked': True
        },
        {
          'code': r"""
          >>> # Testing NinjaAnt strikes all bees in its place
          >>> test_place = colony.places["tunnel_0_0"]
          >>> for _ in range(3):
          ...     test_place.add_insect(Bee(2))
          >>> ninja = NinjaAnt()
          >>> test_place.add_insect(ninja)
          >>> ninja.action(colony)   # should strike all bees in place
          >>> [bee.armor for bee in test_place.bees]
          7ba130373ce1098e546d938c59eedd24
          # locked
          """,
          'hidden': False,
          'locked': True
        }
      ],
      'scored': True,
      'setup': r"""
      >>> from ants import *
      >>> hive, layout = Hive(AssaultPlan()), dry_layout
      >>> dimensions = (1, 9)
      >>> colony = AntColony(None, hive, ant_types(), layout, dimensions)
      """,
      'teardown': '',
      'type': 'doctest'
    }
  ]
}
