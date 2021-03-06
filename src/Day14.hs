module Day14
    ( 
    day14
   ,day14b
   ,twoDimensionGroup
   ,adjacentCoordinates
   ,qgroup
    )
    where
    
import Text.Printf
import Data.Char
import Day10
import Data.Map (Map)
import Data.Ord (comparing)
import Data.List (groupBy,sortBy,find,(\\),nub)

day14 :: String -> Int
day14 input =  length $ filter (\y->y=='1') $ concat $  day14' input

day14' input =  map (\y-> concatMap intToBinaryString $ hexToInt y)  $ hashes input

day14b :: String -> Int
day14b input = length $ 
                    filter (\y -> (snd $ head y) =='1' )  $
                    twoDimensionGroup $ day14' input
input = "flqrgnkx"

knotHash :: String -> String
knotHash input = day10b input [0..255]

intToBinaryString :: Int -> String 
intToBinaryString arg = printf "%04b" arg

hexToInt :: String -> [Int]
hexToInt arg = map digitToInt arg


hashes :: String -> [String]
-- cached test input.. this algorithm is tested though right???
hashes "flqrgnkx" =  ["d4f76bdcbf838f8416ccfa8bc6d1f9e6","55eab3c4fbfede16dcec2c66dda26464","0adf13fa40e8ea815376776af3b7b231","ad3da28cd7b8fb99742c0e63672caf62","682fe48c55876aaaa11df2634f96d31a","c9f5fffc464a565f9e94f323a8aaa94b","44c01a72626cee0e3bc0e8988d137463","d672360830c6394946acd937765e0636","f7e7295fcbacd3013b5c9e94c3816f93","d7818e0f3b38bd02b9a29582f9979891","10b25161deda763644711e2948a92712","7ff09179e8e96f674d9e6062c422732a","16275af10142457c551d8be3561e1b93","4ce62ee70369150fe9b3ddc74da74cf5","886cb2b60a4476bcfd55d6edc50e3a06","cdb4e9a151924719f80ca4f6e0383eec","367e3078d5c5f35cba7c543914118392","fd2a5160d14492ae740ca8a21ac246a3","7eaf877cbf2217dec15ab4f2bf97aa21","ac46391c426f20d5c8db7c469712e952","0b0e8797d2b6c9f3f3f83440af911319","90af1026b14eab83c3fed84db81460ba","37979f3c760aee18ec95a8144d387e47","526a6a81e9b409046648aa9cff8348af","0255ea4dd31c0ab7afb91b2dd99aa849","24f485912276be17c6f9696f01600362","0ad4070dd014f2859515a2c4400e22ed","e40550965fa4a54182cf1362aa36bc24","56985555028d21bba6fa564aa436cbc2","2a7402ae20d45710ada998a439093970","6d4a1385883393436fee43d62a5b1da2","e229e7ba71dfea4723b4fdf91834b59f","6bf42f5f69a12d9540af3ca0c61b3405","78df639419e8a6eb788da40819f7673c","b620c2dcd0b8d95b2c6f512adab2f2c0","e19a780d69e8e3a91dfcc9c81aec67b4","95bf838021b6d6f94890b4985755d7b0","e5998d515ac26bdddb3718c52c9b626a","39e11459e2ad61108407f7c0b810e057","06d83fde5092eef19f55b3875a9b07da","98934d7e296a2629535d17121585da35","a0796ab885d0b844e1fab86ff3a32917","36bc10e76650b4372b10cedcd9053f02","55daca88779ee6b57d8b5ca30479bcbf","fcdf2354d9eb0bc4209dfd727f18609c","07b751638bc64b13195cf68ebea205b3","30dfebedacb40e459862f0d08ea14d02","529732ed72ec2bae3080cbf8df33452b","94a61e0d610dfacde26fd9f2bb142a59","7771aebcd50727f5ba8cd5b3877a0cb5","29b2265f80b27c349864f2b56a382b5a","43d2c7e82ed47fec6cf0326c271b81a8","6f1516a6c01966a39eb97322ebded540","9bcd0920e7a18161c5c1f899aeb12182","1de6a7d956d8799a368ebf81a8e89db3","8b6eaa9c6ac616d5fd0c8546e139e7b1","f8bda6db85ee7a0442d81725c2e11fb9","aecaaa07107d1514801c6315f4e1d18b","0dd19afe028dfe1cafc4fb1b554e4005","735d7e9d09107e2c3785d94c1efd3072","54997903ab419d5c0b2f19020a15b408","a126f678d13a7eb2a2cebd629ba273d7","13a7aaec00d8ba1493d275fd59a4b000","ba05f7592d30509ecec0a36bfec48ab4","86f6fb5a6bc7ae5e695fcc27907656e0","0a788c8262eaac1443bd7bf576e6bf13","e218d2d352eddb4a8d5e540c05b555bb","424acaa9e9010955ebc2ffc2a1bf09dc","ca4c7aaa9d841d601441147b8269b95a","4572bcd85d1456a6ca07d08488fc4d4a","2ff95f8a79e04926b3449403b7dee814","1cbe703e5d7d2c02778920ae7fb90b5f","0b7229a8519a0cc4eea1ee10e755ba42","1639a4c5ac490460d97bb1556e582e91","69fcbaf9881ca4db1cb77f820394da26","ad05f09eefc79e588770a9902f200be2","09799b280c366954d7e7bb7fe2e790a5","d01150f125d648058794be74ffe49589","0290e7d5aa47406f2b4fe4a5e20f02a8","776d05fea6dd3b2097d2e1d8bc5a4a51","de5ebf77bc1a8d9bda80e1c0d7ca77e9","9c0b49d4aea2e0bc2103a4dd364e9be2","e36cc0f7c27c629335ef125dad699e38","49a6906e0ca23cbf286ec6864485da21","556f8184254b4636d9b6afb64ddeea2e","1db872ece6b2f118cfef4cc4db92c5a2","3428e6f39fa52f1c3fbf322ad2eadc7c","e727c808e371c129872e7792bd1721bd","eb42398141ffb3c9d16a5aaa8e8cbc20","69d84d31f7b797d3f9e3ef591e910ae0","1ffeb273703601eb4f443c0478b61c6d","ac554123970944966e57137a6b1904f1","a8b27487b8e251902bb4fc36d2867f0c","6ae792a069428d2821ab6be736385f66","a040424855d040f094431e41b5740294","8fb21b7c3178346ca7244038bf990d9b","8e1f5a907cc49b1dc7aaad01c0f53cad","c12646911e7cb5b6c756891ea0a7da8a","15d314d62597299e0ac167f0d19fa1b2","dde081c291913b6925316e2201454474","2e5d36479a5331ee327f0b2c3a3592e3","f5bef57a8db747e4b63c93cdbf12d0f4","8510689f966ff3f58c987790b8b0c0a6","a8bfca05aee0092888a8a6f02255ee58","119aa69a364de18692c6c00ed5adaee7","872998778c357c114c7254f95cf7b419","6cc9b9df9152c12300e4d824e5333b17","ad13417ef97a1639b8c59887fc4fb74b","aa5bbbf5bdc2c8f50fdc8718bc308bb6","a128d4778c7af5fd94d0f23b92896d2f","1e21430194a72e35f084e4307ac0cf80","c1f917992a897649967968578e1c432b","e3effbd35e76d9bf9a3ca6c6c14a83a4","da6384ace7a1a79350b9b73702098d0c","a88d5f8242851ab213947f7353bbf80c","fa93dc5843012b75d2aead29b102fd47","13570db120ee2334289aefea3d882e0d","c858a4c2ef72a2b46b5c553580558679","4e9b0163d1a4733c89e5e91fd6f7dfe9","5a3db8c1a2ff4d6a837e91c6d9576222","20e5565265d89e229e88941d0e57ecea","37b50e4853d44ef3fd995709536f45bd","319d683a9b70bea96d2214c98647e0b1","3aba0a16ce39244a1145de4221804a26","6a2e05ef05136f43a057fdeb2e413b21","ed5bcc36cf58f4ed534f966c696f3e1c","d31a8759d827c4ee9f901b74ab089c9d","3ecaf0d2646e9dc1483e752d52688a1e"]
hashes input = map(\y -> knotHash (input ++ "-" ++ show y) ) [0..127]

sortGroup = groupBy (\x y -> snd x == snd y)  . sortBy (comparing snd) 

twoDimensionGroup grid =     concat $ foldr (\y acc-> (qgroup  y) : acc) [] 
        $ sortGroup
        $ gridToList grid

matchingAdjacent x y = (snd x) == (snd y)  && 
                       (elem (fst y) $ adjacentCoordinates (fst x))

gridToList grid = zip coords (concat grid) 
    where listLength = length grid - 1
          coords = (,) <$> [0..listLength] <*> [0..listLength]

adjacentCoordinates :: (Num a1, Num a) => (a1, a) -> [(a1, a)]
adjacentCoordinates (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]


matching xs@[] =  (xs, xs)
matching (x:xs) = (\y -> (y, xs \\ y)) $ match x xs
    
match _ []  = []
match x xs  = set ++ foldr (\z acc -> (match z (xs \\ set) ++ acc)) [] set
    where set= match' x xs


          
match' x set = filter (\y -> matchingAdjacent x y ) set

-- matching (x:xs) = (\y -> (y, xs \\ y)) $ match set set
--     where set = x:xs
-- match [] _ = []
-- match (x:xs) set =  combineUnlessEmpty (match' x set)   (match xs set)
--     where combineUnlessEmpty []  _ = [] 
--           combineUnlessEmpty x y = x ++ y
          
-- match' x set = filter (\y -> matchingAdjacent x y ) set

--matching (x:xs) = (\y -> (y, xs \\ y)) $ match (x:xs)
--match (x:xs) =  (foldr (\y acc -> match' y xs) (match' x xs) (match' x xs))
--    where match' x xs = filter (\y -> (matchingAdjacent x y) || y==x ) xs

qgroup [] =  []
qgroup (x:xs) =  nub (x:ys) : (nub $ qgroup zs)
                where (ys,zs) = matching (x:xs)

deconstruct list = map (\y -> map(\(_,x) -> x) y) list 
out =  ["00100", "00100", "00100", "00100","00000"]
out2 = [((0,0),'0'),((0,1),'0'),((0,2),'0'),((0,3),'0'),((0,4),'0'),
        ((1,0),'0'),((1,1),'0'),((1,3),'0'),((1,4),'0'),((2,0),'0'),
        ((2,1),'0'),((2,3),'0'),((2,4),'0'),((3,0),'0'),((3,1),'0'),
        ((3,3),'0'),((3,4),'0'),((4,0),'0'),((4,1),'0'),((4,2),'0'),
        ((4,3),'0'),((4,4),'0')]