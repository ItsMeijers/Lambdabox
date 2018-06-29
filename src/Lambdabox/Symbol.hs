module Lambdabox.Symbol where

class Symbol a where

-- | Bootstrap based on an API Call
data BTC   = BTC deriving Show
data KEY   = KEY deriving Show
data ETH   = ETH deriving Show
data EOS   = EOS deriving Show
data TRX   = TRX deriving Show
data ONT   = ONT deriving Show
data XRP   = XRP deriving Show
data ICX   = ICX deriving Show
data BNB   = BNB deriving Show
data BCC   = BCC deriving Show
data ADA   = ADA deriving Show
data XLM   = XLM deriving Show
data NEO   = NEO deriving Show
data LTC   = LTC deriving Show
data VEN   = VEN deriving Show
data ARN   = ARN deriving Show
data IOTA  = IOTA deriving Show
data XMR   = XMR deriving Show
data NPXS  = NPXS deriving Show
data TUSD  = TUSD deriving Show
data THETA = THETA deriving Show
data BQX   = BQX deriving Show
data NANO  = NANO deriving Show
data ETC   = ETC deriving Show
data LUN   = LUN deriving Show
data INS   = INS deriving Show
data WAVES = WAVES deriving Show
data ZRX   = ZRX deriving Show
data ZIL   = ZIL deriving Show
data WAN   = WAN deriving Show
data QKC   = QKC deriving Show
data DATA  = DATA deriving Show
data GTO   = GTO deriving Show
data MCO   = MCO deriving Show
data DASH  = DASH deriving Show
data CMT   = CMT deriving Show
data SNT   = SNT deriving Show
data GVT   = GVT deriving Show
data AION  = AION deriving Show
data MDA   = MDA deriving Show
data QLC   = QLC deriving Show
data ENG   = ENG deriving Show
data STRAT = STRAT deriving Show
data EDO   = EDO deriving Show
data BRD   = BRD deriving Show
data XVG   = XVG deriving Show
data IOTX  = IOTX deriving Show
data IOST  = IOST deriving Show
data QTUM  = QTUM deriving Show
data ELF   = ELF deriving Show
data NAV   = NAV deriving Show
data NCASH = NCASH deriving Show
data NAS   = NAS deriving Show
data BCN   = BCN deriving Show
data WTC   = WTC deriving Show
data SUB   = SUB deriving Show
data WABI  = WABI deriving Show
data CVC   = CVC deriving Show
data HSR   = HSR deriving Show
data LOOM  = LOOM deriving Show
data MTL   = MTL deriving Show
data AGI   = AGI deriving Show
data NXS   = NXS deriving Show
data OMG   = OMG deriving Show
data AE    = AE deriving Show
data BCPT  = BCPT deriving Show
data AMB   = AMB deriving Show
data REP   = REP deriving Show
data LSK   = LSK deriving Show
data RCN   = RCN deriving Show
data SKY   = SKY deriving Show
data BAT   = BAT deriving Show
data GNT   = GNT deriving Show
data POA   = POA deriving Show
data MAN   = MAN deriving Show
data POWR  = POWR deriving Show
data SNM   = SNM deriving Show
data SC    = SC deriving Show
data ZEC   = ZEC deriving Show
data BLZ   = BLZ deriving Show
data VIB   = VIB deriving Show
data BTG   = BTG deriving Show
data WINGS = WINGS deriving Show
data NULS  = NULS deriving Show
data PPT   = PPT deriving Show
data XEM   = XEM deriving Show
data WPR   = WPR deriving Show
data STEEM = STEEM deriving Show
data YOYO  = YOYO deriving Show
data DGD   = DGD deriving Show
data NEBL  = NEBL deriving Show
data STORM = STORM deriving Show
data XZC   = XZC deriving Show
data KNC   = KNC deriving Show
data BTS   = BTS deriving Show
data ENJ   = ENJ deriving Show
data LEND  = LEND deriving Show
data QSP   = QSP deriving Show
data GAS   = GAS deriving Show
data APPC  = APPC deriving Show
data TRIG  = TRIG deriving Show
data SALT  = SALT deriving Show
data FUN   = FUN deriving Show
data KMD   = KMD deriving Show
data ARK   = ARK deriving Show
data ADX   = ADX deriving Show
data CND   = CND deriving Show
data POE   = POE deriving Show
data RLC   = RLC deriving Show
data ZEN   = ZEN deriving Show
data REQ   = REQ deriving Show
data CDT   = CDT deriving Show
data TNT   = TNT deriving Show
data SYS   = SYS deriving Show
data VIA   = VIA deriving Show
data RPX   = RPX deriving Show
data LRC   = LRC deriving Show
data LINK  = LINK deriving Show
data TNB   = TNB deriving Show
data OST   = OST deriving Show
data CLOAK = CLOAK deriving Show
data AST   = AST deriving Show
data PIVX  = PIVX deriving Show
data FUEL  = FUEL deriving Show
data GXS   = GXS deriving Show
data SNGL  = SNGL deriving Show
data VIBE  = VIBE deriving Show
data BNT   = BNT deriving Show
data DNT   = DNT deriving Show
data MTH   = MTH deriving Show
data CHAT  = CHAT deriving Show
data GRS   = GRS deriving Show
data DLT   = DLT deriving Show
data EVX   = EVX deriving Show
data STORJ = STORJ deriving Show
data MOD   = MOD deriving Show
data ICN   = ICN deriving Show
data RDN   = RDN deriving Show
data OAX   = OAX deriving Show
data BCD   = BCD deriving Show

instance Symbol BTC
instance Symbol KEY
instance Symbol ETH
instance Symbol EOS
instance Symbol TRX
instance Symbol ONT
instance Symbol XRP
instance Symbol ICX
instance Symbol BNB
instance Symbol BCC
instance Symbol ADA
instance Symbol XLM
instance Symbol NEO
instance Symbol LTC
instance Symbol VEN
instance Symbol ARN
instance Symbol IOTA
instance Symbol XMR
instance Symbol NPXS
instance Symbol TUSD
instance Symbol THETA
instance Symbol BQX
instance Symbol NANO
instance Symbol ETC
instance Symbol LUN
instance Symbol INS
instance Symbol WAVES
instance Symbol ZRX
instance Symbol ZIL
instance Symbol WAN
instance Symbol QKC
instance Symbol DATA
instance Symbol GTO
instance Symbol MCO
instance Symbol DASH
instance Symbol CMT
instance Symbol SNT
instance Symbol GVT
instance Symbol AION
instance Symbol MDA
instance Symbol QLC
instance Symbol ENG
instance Symbol STRAT
instance Symbol EDO
instance Symbol BRD
instance Symbol XVG
instance Symbol IOTX
instance Symbol IOST
instance Symbol QTUM
instance Symbol ELF
instance Symbol NAV
instance Symbol NCASH
instance Symbol NAS
instance Symbol BCN
instance Symbol WTC
instance Symbol SUB
instance Symbol WABI
instance Symbol CVC
instance Symbol HSR
instance Symbol LOOM
instance Symbol MTL
instance Symbol AGI
instance Symbol NXS
instance Symbol OMG
instance Symbol AE
instance Symbol BCPT
instance Symbol AMB
instance Symbol REP
instance Symbol LSK
instance Symbol RCN
instance Symbol SKY
instance Symbol BAT
instance Symbol GNT
instance Symbol POA
instance Symbol MAN
instance Symbol POWR
instance Symbol SNM
instance Symbol SC
instance Symbol ZEC
instance Symbol BLZ
instance Symbol VIB
instance Symbol BTG
instance Symbol WINGS
instance Symbol NULS
instance Symbol PPT
instance Symbol XEM
instance Symbol WPR
instance Symbol STEEM
instance Symbol YOYO
instance Symbol DGD
instance Symbol NEBL
instance Symbol STORM
instance Symbol XZC
instance Symbol KNC
instance Symbol BTS
instance Symbol ENJ
instance Symbol LEND
instance Symbol QSP
instance Symbol GAS
instance Symbol APPC
instance Symbol TRIG
instance Symbol SALT
instance Symbol FUN
instance Symbol KMD
instance Symbol ARK
instance Symbol ADX
instance Symbol CND
instance Symbol POE
instance Symbol RLC
instance Symbol ZEN
instance Symbol REQ
instance Symbol CDT
instance Symbol TNT
instance Symbol SYS
instance Symbol VIA
instance Symbol RPX
instance Symbol LRC
instance Symbol LINK
instance Symbol TNB
instance Symbol OST
instance Symbol CLOAK
instance Symbol AST
instance Symbol PIVX
instance Symbol FUEL
instance Symbol GXS
instance Symbol SNGL
instance Symbol VIBE
instance Symbol BNT
instance Symbol DNT
instance Symbol MTH
instance Symbol CHAT
instance Symbol GRS
instance Symbol DLT
instance Symbol EVX
instance Symbol STORJ
instance Symbol MOD
instance Symbol ICN
instance Symbol RDN
instance Symbol OAX
instance Symbol BCD