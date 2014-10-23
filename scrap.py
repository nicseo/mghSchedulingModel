import copy

class Thing:

    def __init__(self,items):
        self.items = items

    def test(self):
        dummyThing = copy.deepcopy(self)
        dummyThing.items.pop()
        dummyThing.items.append(100)
        return dummyThing.items
        
