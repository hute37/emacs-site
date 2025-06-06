;;; .emacs --- init config -*- lexical-binding: t; -*-

;; Author: ht37 <hute37@gmail.com>
;; URL: https://github.com/hute37/emacs-site

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; local custom config variables
;; override in ~/.emacs or in ~/.emacs-local.el

;;; Code:
;;; $Id: .emacs,v 1.6 2005-12-05 03:06:24 hute37 Exp $

;;;  
;;; $Revision: 1.6 $
;;;  
;;; $Log: .emacs,v $
;;; Revision 1.6  2005-12-05 03:06:24  hute37
;;; ntemacs update
;;;
;;; Revision 1.4  2005/11/30 23:18:58  hute37
;;; upd
;;;
;;; Revision 1.3  2005/11/27 19:07:17  hute37
;;; kkv
;;;
;;; Revision 1.2  2005/11/27 19:05:39  hute37
;;; new configured dot
;;;
;;;
;;;

;; '(Info-additional-directory-list (quote ("D:\\usr\\share\\info")))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(load "~/.emacs-start")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highligh-search nil t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(auto-compression-mode t nil (jka-compr))
 '(blink-matching-paren-on-screen t)
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-generic-program "firefox")
 '(calendar-week-start-day 1)
 '(case-fold-search t)
 '(chatgpt-shell-model-versions
   '("gpt-3.5-turbo-16k-0613" "gpt-3.5-turbo-16k" "gpt-3.5-turbo-0613"
     "gpt-3.5-turbo"))
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(cperl-clobber-lisp-bindings t)
 '(cperl-electric-keywords t)
 '(cperl-electric-lbrace-space t)
 '(cperl-electric-linefeed t)
 '(cperl-electric-parens t)
 '(cperl-font-lock t)
 '(cperl-hairy t)
 '(cperl-lazy-help-time 3)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "English")
 '(cursor-type 'box)
 '(custom-enabled-themes '(ef-eagle))
 '(custom-safe-themes
   '("b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a"
     "2551f2b4bc12993e9b8560144fb072b785d4cddbef2b6ec880c602839227b8c7"
     "a0e9bc5696ce581f09f7f3e7228b949988d76da5a8376e1f2da39d1d026af386"
     "da69584c7fe6c0acadd7d4ce3314d5da8c2a85c5c9d0867c67f7924d413f4436"
     "541282f66e5cc83918994002667d2268f0a563205117860e71b7cb823c1a11e9"
     "3d9938bbef24ecee9f2632cb25339bf2312d062b398f0dfb99b918f8f11e11b1"
     "71b688e7ef7c844512fa7c4de7e99e623de99a2a8b3ac3df4d02f2cd2c3215e7"
     "b3ba955a30f22fe444831d7bc89f6466b23db8ce87530076d1f1c30505a4c23b"
     "fae5872ff90462502b3bedfe689c02d2fa281bc63d33cb007b94a199af6ccf24"
     "19b62f442479efd3ca4c1cef81c2311579a98bbc0f3684b49cdf9321bd5dfdbf"
     "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2"
     "cee5c56dc8b95b345bfe1c88d82d48f89e0f23008b0c2154ef452b2ce348da37"
     "ffa78fc746f85d1c88a2d1691b1e37d21832e9a44a0eeee114a00816eabcdaf9"
     "90185f1d8362727f2aeac7a3d67d3aec789f55c10bb47dada4eefb2e14aa5d01"
     "4c16a8be2f20a68f0b63979722676a176c4f77e2216cc8fe0ea200f597ceb22e"
     "aff0396925324838889f011fd3f5a0b91652b88f5fd0611f7b10021cc76f9e09"
     "c038d994d271ebf2d50fa76db7ed0f288f17b9ad01b425efec09519fa873af53"
     "6af300029805f10970ebec4cea3134f381cd02f04c96acba083c76e2da23f3ec"
     "ac893acecb0f1cf2b6ccea5c70ea97516c13c2b80c07f3292c21d6eb0cb45239"
     "b1791a921c4f38cb966c6f78633364ad880ad9cf36eef01c60982c54ec9dd088"
     "d609d9aaf89d935677b04d34e4449ba3f8bbfdcaaeeaab3d21ee035f43321ff1"
     "df39cc8ecf022613fc2515bccde55df40cb604d7568cb96cd7fe1eff806b863b"
     "d6b369a3f09f34cdbaed93eeefcc6a0e05e135d187252e01b0031559b1671e97"
     "59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b"
     "296dcaeb2582e7f759e813407ff1facfd979faa071cf27ef54100202c45ae7d4"
     "211621592803ada9c81ec8f8ba0659df185f9dc06183fcd0e40fbf646c995f23"
     "ea4dd126d72d30805c083421a50544e235176d9698c8c541b824b60912275ba1"
     "1ad12cda71588cc82e74f1cabeed99705c6a60d23ee1bb355c293ba9c000d4ac"
     "cd5f8f91cc2560c017cc9ec24a9ab637451e36afd22e00a03e08d7b1b87c29ca"
     "a3a71b922fb6cbf9283884ac8a9109935e04550bcc5d2a05414a58c52a8ffc47"
     "ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
     "00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729"
     "36c5acdaf85dda0dad1dd3ad643aacd478fb967960ee1f83981d160c52b3c8ac"
     "e85a354f77ae6c2e47667370a8beddf02e8772a02e1f7edb7089e793f4762a45"
     "8a3d04fd24afde8333c1437a3ecaa616f121554041a4e7e48f21b28f13b50246"
     "9ddb83c12595e789e9abd04a5c0705661748776223a794a6f64669352b956e79"
     "1a5bf8692b9aaa73a9a29bf8895546cfc06c0e064e4306f7f78a8f8437502322"
     "0f08efc35f1190204ac227e8c866b18400612d2137e2d13dcbf4693953681ff3"
     "63df1d9126e997cd7f6ef5f7dd61312b0f65bcb18974e199e82d0e72b8576612"
     "eb93ba33f178940853271e32f889ad56e5cfece616cc14e776429cb02870bd26"
     "5c7436d940c0b2e35e4aa201459159075bb7b5b7a0b6d595376267484fe6dcfc"
     "bd82a9cabe0cb7edd0bd33f696e140ca331af235baab8b268fb2d098b0e366ce"
     "47e0d1392558cdb3a1a73833f7609c526809e5859755c9042cc24aea6a8263c8"
     "bf2a27372858a4ca36afdb4bc3341d31801523c3016e404553e630e362232027"
     "83afe95f30785201eb9e189bf507b0f38076a436804c987c52830214ac025906"
     "52645403935dd6f8266c3a8f831949c1e79155324efdf709b5af0d078cd6c822"
     "83f4659ff482028561aed742320eaa491b1416c7c43181647f0c8ef256610dca"
     "42f5c59e0b2aabac84a661586a939d8c459f658fae72887b46a8496389ef6b7e"
     "e103856e345842bf4334b3db8b773ad01153ce02a3d6655d7c6f5cdb9558c62c"
     "aee6debe7b326de2968d8b023fdc9ee7e6c9996a80532186674f2e1376ad1782"
     "63865ac3d0f7fe970be87f4690ed3c19be51632808f65e6fdd812e74f98b95fa"
     "d16a286583f7326bb1e3baf7d83df3643533cfa9ac6f0601d1b4a595ad2db523"
     "8d371c95370c5965746ccddd94fe96d7a2fbcdd67a77b3951db6fb11f68baf7d"
     "be5ebe527c28434cd4bdf33b0430a4eec557e59a63b632695a7ce7014c6713a7"
     "f87c245f58040e87eba4a7383c6fcf9ca7922e57f2cd71c366176d60efcf305a"
     "234c3805fb341b7ce2a9e8ce6d72dba9b81e9335422cfee838e128edfb8a9774"
     "578db0ce196a4c849aa995a0dd32d5fe85da59a1ec303614aa40a28bf9ad8b99"
     "cb4c6fef7d911b858f907f0c93890c4a44a78ad22537e9707c184f7e686e8024"
     "47772b7cb9a4d978fa72eb1e895b0d84ecfc0baa670ab539e64c43135eeec79c"
     "90a6936b8c8f709825a0165928ef95f24f79486805be787630e7fe46bc5c9c7f"
     "f33f145b036bc630b5a7e2a3100fd38c4220699347e32283a74489b19c36c84b"
     "8f85f336a6d3ed5907435ea196addbbbdb172a8d67c6f7dbcdfa71cd2e8d811a"
     "9ac7d7994276098c96854480c123aaea66e059a23148dd0a80024f91a1db94f6"
     "5595d75552f01ea5cf48cf21cab93793362d71b69ad6ce4ab1662506183b16b5"
     "b69d8a1a142cde4bbde2f940ac59d2148e987cd235d13d6c4f412934978da8ab"
     "ba20c8541f529565f6c735fc5fc4219cc6938b2dce1d3eba41ebcf288c2971e0"
     "02f30097c5840767499ed2f76d978545f6fb3a1c3ba59274576f7e23fc39d30b"
     "3a496bf8eafa4dfe4db359b964f559066aafb516898f63fdf450188133112c7b"
     "d678ec420b0ede7ace7adb0fa9f448329e132de2f868b20773e282eb29fb1498"
     "df85955fd38ee2dae7476a5fa93e58e594df96132871c10ecaf4de95bdae932a"
     "738c4838957c1884dfacbb6f4f783c54e87c4a6b31c336d6279fc1c2b2ee56c5"
     "2eb6dd78a83cdecc2ea783a2640f4c316f41e223353d4a32fe936113e8a2ddbf"
     "0ac7d13bc30eac2f92bbc3008294dafb5ba5167f2bf25c0a013f29f62763b996"
     "0cd00c17f9c1f408343ac77237efca1e4e335b84406e05221126a6ee7da28971"
     "5078e1845735a69b21b5effe083998dc368853320f449530c2616cf70bc3c47b"
     "3a2e0c5597f6d74d99daa2b5bbbc2a653d02d6b88fcd73d3c84ebf25cde37b3f"
     "45e76a1b1e3bd74adb03192bf8d6eea2e469a1cf6f60088b99d57f1374d77a04"
     "6faced0e0c041c281f65db81ea3221e373d61fd91014275cc8ca79e4fb3316b7"
     "18cd5a0173772cdaee5522b79c444acbc85f9a06055ec54bb91491173bc90aaa"
     "f0c94bf6a29c232300e46af50f46ce337e721eacca6d618e8654a263db5ecdbe"
     "dc8ad8b5833ae06e373cc3d64be28e67e6c3d084ea5f0e9e77225b3badbec661"
     "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693"
     "08381598f21dba085a40591f9ac663096582fb12ea1a49af61a2742813d310b3"
     "0793aa8ee42561496b59f019fb09e0731962570f0e444b09f8c1ebd177a22471"
     "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509"
     "2ed177de0dfc32a6a32d6109ddfd1782a61bcc23916b7b967fa212666d1aa95c"
     "6b234feec8db588ad5ec2a9d9d7b935f7a155104b25ccfb94d921c45a2ff7d22"
     "16ab866312f1bd47d1304b303145f339eac46bbc8d655c9bfa423b957aa23cc9"
     "4eb69f17b4fa0cd74f4ff497bb6075d939e8d8bf4321ce8b81d13974000baac1"
     "021321ae56a45794f43b41de09fb2bfca184e196666b7d7ff59ea97ec2114559"
     "3263bd17a7299449e6ffe118f0a14b92373763c4ccb140f4a30c182a85516d7f"
     "6ec768e90ce4b95869e859323cb3ee506c544a764e954ac436bd44702bd666c0"
     "97fbd952a3b01fbace2aa49b3b07692cacc3009883c7219b86e41669c2b65683"
     "39fe48be738ea23b0295cdf17c99054bb439a7d830248d7e6493c2110bfed6f8"
     "7beac4a68f03662b083c9c2d4f1d7f8e4be2b3d4b0d904350a9edf3cf7ce3d7f"
     "f5e72fe6414e887b88794ff535a861ffe9caaf4ac36e070b2bbd6ec4465dbb93"
     "b375fc54d0c535bddc2b8012870008055bf29d70eea151869e6ad7aaaadb0d24"
     "42ec9eaa86da5f052feed0e35b578681015b9e21ab7b5377a5a34ea9a0a9e1b9"
     "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa"
     "998975856274957564b0ab8f4219300bca12a0f553d41c1438bbca065f298a29"
     "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db"
     "c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063"
     "2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c"
     "3325e2c49c8cc81a8cc94b0d57f1975e6562858db5de840b03338529c64f58d1"
     "ecc077ef834d36aa9839ec7997aad035f4586df7271dd492ec75a3b71f0559b3"
     "437cd756e079901ccdecd9c397662a3ee4da646417d7469a1c35aa8e246562fe"
     "143d897548e5a7efb5cf92c35bd39fe7c90cbd28f9236225ad3e80e1b79cef8a"
     "03c32698863b38cb07bf7e6a54b6c1de81f752a6c4eab3642749007d5dcf0aef"
     "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97"
     "e01db763cd9daa56f75df8ebd057f84017ae8b5f351ec90c96c928ad50f3eb25"
     "854f0e982e9f46844374b7d72c0137276621db317738281888f15fddb1565aeb"
     "1d993a521b58c12b42f2578ddc5d5094d72fca7bcab19166d6311cafd7aa430a"
     "1cd4df5762b3041a09609b5fb85933bb3ae71f298c37ba9e14804737e867faf3"
     "5ef596398fb0ceee52c269e2f0ab81c74b4322ab4eb2b014f4f4435c75f06534"
     "fd23280005748f3d1e15d2ce612dbe7003d7d551b5debd4287b6eeafd8994413"
     "db152b961f7e6075f226a24bba7faf5b1ff016a0e614afe4e544df5ae2637b3c"
     "db7f422324a763cfdea47abf0f931461d1493f2ecf8b42be87bbbbbabf287bfe"
     "c0f4b66aa26aa3fded1cbefe50184a08f5132756523b640f68f3e54fd5f584bd"
     "0d75aa06198c4245ac2a8877bfc56503d5d8199cc85da2c65a6791b84afb9024"
     "e8a0c94af8c0eeec7ae0f1633d29098ea722e5765f1e9c67b49da6f3414b9bfe"
     "02199888a97767d7779269a39ba2e641d77661b31b3b8dd494b1a7250d1c8dc1"
     "243f7c2a5355c8f65ad7cd9f4ca9dfd37008eaa63221aba71f863db03f71752f"
     "bffb799032a7404b33e431e6a1c46dc0ca62f54fdd20744a35a57c3f78586646"
     "0bff60fb779498e69ea705825a2ca1a5497a4fccef93bf3275705c2d27528f2f"
     "f7ef6451d988d6e2fc86deea398eee02b3371703d88f265d31a011bd240dcf99"
     "bd51a329aa9b8e29c6cf2c8a8cf136e0d2960947dfa5c1f82b29c9178ad89a27"
     "e6ccd0cc810aa6458391e95e4874942875252cd0342efd5a193de92bfbb6416b"
     "ae3a3bed17b28585ce84266893fa3a4ef0d7d721451c887df5ef3e24a9efef8c"
     "08a89acffece58825e75479333109e01438650d27661b29212e6560070b156cf"
     "ace21d57cd9a22c96c021acfd2938411e3374626fe8d91afb9bb969b5269ea75"
     "1a212b23eb9a9bedde5ca8d8568b1e6351f6d6f989dd9e9de7fba8621e8ef82d"
     "a95e86f310e90576a64ef75e5be5d8d8dfc051435af3be59c5f8b5b1b60d82c2"
     "11986184025c9e658eeff90e95ab8e9592f40b3564a5854f9ce1eab1804abd79"
     "9974b640dd56b81b2f1ed620d23f2d9380d7762f55d37c8c19b0670d60fc912a"
     "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3"
     "7675ffd2f5cb01a7aab53bcdd702fa019b56c764900f2eea0f74ccfc8e854386"
     "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2"
     "de65dc21fefce202883a5071170962c73b4bf4b691156d0a28239765f71b23e5"
     "17fab8756a577f28b9ba90ddcfad7210fd10b637cba9c22765eeb5bb43efb3a0"
     "b906ff38296a55463aa5ec2253e2fa1f62fd7389db901dae5ec5ec323f9da400"
     "18816489bf16422e37f0a0501746cd0849177d3f25e8226abcc046437e363fd8"
     "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c"
     "7023f8768081cd1275f7fd1cd567277e44402c65adfe4dc10a3a908055ed634d"
     "0c46a9128995ad772ecbfc5a5193cb253a9a0200bcddf4d6895370e0a92545b4"
     "45f7fec480eb3bdf364cbfcbc8d11ed0228bcf586ce7370fc30a6ce5770f181a"
     "d71f6c718dab453b625c407adc50479867a557668d5c21599a1ebea204d9e4f3"
     "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "3b09eb07767faffb708574c44b9f46a6e2d3248e605cf144a7ca0bc9efd6bcf8"
     "2eade8d22ce8d204b074c42bc1b22fabccc7e8dd0bbdf47e0a233d1c619a6e7b"
     "8a97050c9dd0af1cd8c3290b061f4b6032ccf2044ddc4d3c2c39e516239b2463"
     "1dd7b369ab51f00e91b6a990634017916e7bdeb64002b4dda0d7a618785725ac"
     "e491d84f66d5d540e8459e9af173789d6b70e42a57b2a537823441b6921b39bd"
     "3903f1967cdce48bcf4e9914cdc4f16cd0cd2f09560fe7f2429a8b9f1dfb9c99"
     "1068ae7acf99967cc322831589497fee6fb430490147ca12ca7dd3e38d9b552a"
     "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3"
     "43b219a31db8fddfdc8fdbfdbd97e3d64c09c1c9fdd5dff83f3ffc2ddb8f0ba0"
     "2593436c53c59d650c8e3b5337a45f0e1542b1ba46ce8956861316e860b145a0"
     "2d15316ddb0b94c9c8b4c6cc77ef68486ff738d05bbb6edefec0dca7eafedcae"
     "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6"
     "3e160974b9e3e1b53270d1fb5bbaf56f0c689017e177972f72584bf096efc4cc"
     "f77c6dac9f41948bd392974d153aa338be041445eb0b2d5bd37f7171ae2dc02f"
     "29687181ea85cc2710a49fc3def59a5d5127c44ea5656c78d9047d5d9aaad712"
     "862a0ccc73c12df4df325427f9285fa6a5bbba593a77257f43b01c84269f51b0"
     "ab46e73ba845b2aa5cba1af63efb297651ee8ec674ad3065a8a37438e9321a45"
     "8136cbb3b29b4c86ca3354d85005f527adcf9393b227980fc144a2c24ba63688"
     "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277"
     "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5"
     "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb"
     "0329c772ed96053a73b9ddddf96c1183e23c267955bbdf78e7933057ce9da04b"
     "ebc35c8e71983b8401562900abb28feedf4d8fcdfcdea35b3da8449d78ebecc6"
     "6e32d7aab92ad2ad4d3a915cd9ace5dc1d9d8f0486b785bdb86c79ff5ca0c189"
     "0eccc893d77f889322d6299bec0f2263bffb6d3ecc79ccef76f1a2988859419e"
     default))
 '(cvs-allow-dir-commit t)
 '(default-input-method "latin-1-prefix")
 '(display-buffer-reuse-frames t)
 '(elfeed-sort-order 'ascending)
 '(flycheck-posframe-border-width 5)
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(frame-brackground-mode 'dark)
 '(fringe-mode 6 nil (fringe))
 '(global-auto-revert-mode t nil (autorevert))
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode nil nil (hl-line))
 '(grep-command "egrep -n -e ")
 '(hc-ctrl-backslash-completes-a-la-mode t)
 '(hc-ctrl-x-c-is-completion t)
 '(home-end-enable nil)
 '(hydra-posframe-show-params
   '(:poshandler posframe-poshandler-frame-bottom-center
                 :internal-border-width 15) t)
 '(ibuffer-deletion-face 'dired-flagged)
 '(ibuffer-filter-group-name-face 'dired-mark)
 '(ibuffer-marked-face 'dired-marked)
 '(ibuffer-title-face 'dired-header)
 '(inhibit-startup-screen 1)
 '(ivy-posframe-border-width 15)
 '(ivy-posframe-style 'frame-bottom-window-center)
 '(keypad-numlock-setup 'numeric nil (keypad))
 '(keypad-numlock-shifted-setup 'prefix nil (keypad))
 '(keypad-setup 'cursor nil (keypad))
 '(keypad-shifted-setup 'S-cursor nil (keypad))
 '(linum-format " %7d ")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-use-insert-directory-program nil)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(mode-compile-expert-p t)
 '(mouse-wheel-follow-mouse t)
 '(native-comp-async-report-warnings-errors 'silent)
 '(org-agenda-files
   '("/home/gp21012/Dropbox/Local/data/org/all/task.org"
     "/home/gp21012/Dropbox/Local/data/org/all/milk.org"
     "/home/gp21012/Dropbox/Local/data/org/all/read.org"
     "/home/gp21012/Dropbox/Local/data/org/all/dots.org"
     "/home/gp21012/work/bu/org-info/todo.org"))
 '(package-selected-packages
   '(ace-jump-mode ag all-the-icons-completion auctex-latexmk
                   auto-compile biblio cdlatex citar-denote
                   citar-embark code-cells company-ansible
                   company-auctex company-posframe company-terraform
                   consult-dir consult-lsp consult-notes
                   consult-projectile corfu crux dap-mode dashboard
                   delight denote-menu denote-refs diminish
                   dired-preview docker dockerfile-mode eat
                   edit-server ef-themes ein elfeed-goodies elfeed-org
                   embark-consult engine-mode
                   eshell-syntax-highlighting eshell-toggle ess evil
                   flycheck-haskell general git-gutter-fringe
                   google-this gptel helpful hide-mode-line hl-todo
                   json-mode json-reformat julia-mode julia-repl
                   jupyter lsp-pyright lsp-ui marginalia maxima
                   minions mood-line multi-vterm nov ob-async ob-http
                   ob-restclient olivetti orderless org-autolist
                   org-context org-modern org-noter-pdftools
                   org-tree-slide pandoc-mode pdf-view-restore pdfgrep
                   plantuml-mode pocket-reader poetry poly-R
                   poly-ansible popper puppet-mode py-isort
                   python-black quelpa-use-package ranger regex-tool
                   req-package restclient-jq solaire-mode
                   treemacs-all-the-icons treemacs-icons-dired
                   treemacs-magit treemacs-projectile undo-tree
                   use-package-hydra vdiff vertico vimish-fold
                   visible-mark wgrep wgrep-ag with-venv xterm-color
                   yapfify))
 '(perl-dbg-flags "-c -w -MB::Lint")
 '(ps-line-number t)
 '(ps-paper-type 'a4)
 '(ps-printer-name nil)
 '(recentf-mode t nil (recentf))
 '(safe-local-variable-values
   '((eval setq-local denote-directory
           (expand-file-name "notes/net"
                             (locate-dominating-file default-directory
                                                     ".dir-locals.el")))
     (eval setq-local denote-directory
           (expand-file-name "notes/nat"
                             (locate-dominating-file default-directory
                                                     ".dir-locals.el")))
     (eval setq-local citar-bibliography
           (list
            (expand-file-name "notes/ref/references.bib"
                              (locate-dominating-file
                               default-directory ".dir-locals.el"))))
     (eval setq-local org-cite-global-bibliography
           (list
            (expand-file-name "notes/ref/references.bib"
                              (locate-dominating-file
                               default-directory ".dir-locals.el"))))
     (eval add-to-list 'org-agenda-files (buffer-file-name) 'append)))
 '(save-place-mode t nil (saveplace))
 '(standard-indent 4)
 '(tab-stop-list '(4 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
 '(tag-reuse-buffers nil t)
 '(tag-reuse-window t t)
 '(time-stamp-time-zone "MET")
 '(tool-bar-mode nil)
 '(track-eol t)
 '(truncate-lines t)
 '(twgrep-change-readonly-file t t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil)
 '(vc-cvs-stay-local nil)
 '(warning-suppress-types '((use-package) (use-package)))
 '(wgrep-auto-save-buffer nil)
 '(wgrep-enable-key "e")
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(woman-imenu t)
 '(z-use-helm nil)
 '(z-use-pdf-tools t)
 '(z-use-py-jupyter t)
 '(z-var-denote-directory "~/work/vs/dve-sample-py/notes/nat")
 '(z-var-global-bibliography '("~/work/vs/dve-sample-py/notes/ref/references.bib")))
(set-face-attribute 'mode-line nil  :height 160)
(set-face-attribute 'default nil :height 160)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :slant normal :weight normal :height 160 :width normal :family "JetBrains Mono Medium"))))
;;  '(fixed-pitch ((t (:family "JetBrains Mono Medium"))))
;;  '(variable-pitch ((t (:family "Source Sans Pro" :foundry "ADBE" :slant normal :weight semi-bold :height 165 :width normal)))))
;; (custom-theme-set-faces
;;    'user
;;    '(variable-pitch ((t (:family "Source Sans Pro" :foundry "ADBE" :slant normal :weight semi-bold :height 165 :width normal))))
;;    '(fixed-pitch ((t ( :family "JetBrains Mono Medium")))))

;;(color-theme-z-gnome2)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Curly" :foundry "BE5N" :slant normal :weight medium :height 151 :width normal))))
 '(fixed-pitch ((t (:family "Iosevka Curly" :foundry "BE5N" :slant normal :weight medium :height 151 :width normal))))
 '(variable-pitch ((t (:family "Fira Sans Condensed" :foundry "CTDB" :slant normal :weight semi-bold :height 165 :width normal)))))

