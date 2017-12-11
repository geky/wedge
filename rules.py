

class Unexpected(Exception):
    def __init__(self, matcher, token, rule):
        self.matcher = matcher
        self.token = token
        self.rule = rule

    def __str__(self):
        return "unexpected %r looking for %r" % (self.token, self.rule)

class Matcher:
    def __init__(self, matcher):
        self.matcher = matcher
        self.match = None
        self.failures = []

    def expect(self, rule):
        if callable(rule) and not isinstance(rule, type):
            self.match = rule(self)
        else:
            self.match = self.matcher(self, rule)
        self.failures = []
        return self.match

    def accept(self, rule):
        try:
            self.expect(rule)
            return self.match or True
        except Unexpected as e:
            if e.matcher != self:
                raise
            self.match = None
            self.failures.append(e)
            return False

    def unexpected(self, token=None, rule=None):
        if not token and self.failures:
            return Unexpected(self, self.failures[0].token,
                [e.rule for e in self.failures])
        else:
            return Unexpected(self, token, rule)


def choice(rules):
    def choicerule(m):
        for rule in rules:
            if m.accept(rule):
                return m.match
        else:
            raise m.unexpected()

    return choicerule

def chain(rules):
    def chainrule(m):
        return [m.expect(rule) for rule in rules]

    return chainrule

def optional(rule):
    def optionalrule(m):
        m.accept(rule)
        return m.match

    return optionalrule

def many(rule):
    def manyrule(m):
        results = []
        while m.accept(rule):
            results.append(m.match)
        return results

    return manyrule

def many1(rule):
    def many1rule(m):
        return [m.expect(rule)] + m.expect(many(rule))

    return many1rule

def sepby(rule, sep):
    def sepbyrule(m):
        results = []

        if not m.accept(rule):
            return results
        results.append(m.match)

        while True:
            if not m.accept(sep):
                return results
            results.append(m.expect(rule))

    return sepbyrule

def sepby1(rule, sep):
    def sepby1rule(m):
        results = []
        results.append(m.expect(rule))
        if m.accept(sep):
            results.extend(m.expect(sepby(rule, sep)))
        return results

    return sepby1rule

def endby(rule, sep):
    def endbyrule(m):
        results = []
        while True:
            if not m.accept(rule):
                return results
            results.append(m.match)
            m.expect(sep)

    return endbyrule

def endby1(rule, sep):
    def endby1rule(m):
        results = []
        results.append(m.expect(rule))
        m.expect(sep)
        results.extend(m.expect(endby(rule, sep)))
        return results

    return endby1rule
