## register
username + passwd

## login
username + passwd

## sendMobile
auth + mobile

## bindmobile
auth + mobile + code

## games list
auth

## game play type
auth + gameType

## deposit
auth + amount + channel

## bet
auth + gameType + betCont{[playType + count],[playType + count]} + traceInfo{isTrace + traceCount}

## orders
auth + pageNo + pageSize

## orderDetail
auth + orderId

## trace orders
auth + pageNo + pageSize

## trace Detail
auth + orderId

## cancel trace
auth + orderId

## withdraw
auth + amount

## get withdraw channel
auth

## add withdraw channel
auth + channel + drawCont{alipay:[account], wechat:[account], card:[cardNo, cardName, bankType]}

## user's withdraw account list
auth

## remove withdraw account
auth + id

## logout
auth

## updatepass
auth + oldpass + newpass

## updatewithdraw pass
