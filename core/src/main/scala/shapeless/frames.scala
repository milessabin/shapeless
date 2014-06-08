/*
 * Copyright (c) 2013 Arseniy Zhizhelev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless

import shapeless.ops.frame.SlotC

/**
 * Interface for strictly typed frames. They differ from extensible records in that they are not extensible. 
 * They contain exactly the given set of slots with values. 
 * 
 * The difference from lenses is that frame approaches the same problem from the other side. It changes storage from case class to a map. 
 * This approach has it's own advantages and disadvantages.
 * 
 * Slot - is a typed field of some FrameInstance.
 * Frame - is a sequence of slots. It is strictly typed. The Frame contains metainformation about frame instances.
 * FrameInstance - is an "object" of "type", defined by Frame.
 * 
 * TypedFrame - a Frame associated with a case class. 
 * <CustomCaseClass> with FrameInstance - a case class that is compatible with frames.   
 * 
 * @author Arseniy Zhizhelev
 */
object frame {
	import ops.frame._

	/** A slot without generic type argument. */
	trait Slot {
		type Type
		def initialValue: Type
	}

	/** Slot constructor with generic type argument.*/
	def slot[T](initialValue0: T):Slot{type Type = T} = 
		new SlotC(initialValue0)
	
	/** Frame is a strictly typed sequence of slots. */
	sealed trait Frame {		
		type TFrame <: this.type
		/** Set of slots in this frame. */
		type AllSlots 
		def frame:TFrame = this.asInstanceOf[TFrame]
		def slots:Set[Slot]
	}
	/** Initial empty frame. */
	object EmptyFrame extends Frame {
		type TFrame = this.type
		type AllSlots = Any
	
		/** Compose the frame with the slot*/
		def `;` [SlotT <: Slot](slotId: SlotT) =
			FrameKWithSlot(slotId, this)
	
		def slots:Set[Slot] = Set()
	}
	
	final case class FrameKWithSlot[
	    SlotT <: Slot,
			+FrameWithoutNewSlot <: Frame](
	      slotId: SlotT,
	      tail: FrameWithoutNewSlot) extends Frame {
		type TFrame = this.type
		type AllSlots = SlotT with tail.AllSlots
	
		/** Compose the frame with the slot. */
		def `;`[SlotT2 <: Slot](slotId2: SlotT2) =
			FrameKWithSlot[SlotT2, this.type](slotId2, this)
	
		lazy val slots:Set[Slot] = tail.slots + slotId
	}
	
	implicit def frameOps[TFrame<:Frame](frame:TFrame) : FrameOps[TFrame] = new FrameOps(frame)
	

	trait FrameInstance[+TFrame <: Frame] {
		val frame: TFrame
		/** Apply has compile-time protection from accessing slots that are absent in the frame.*/
		def apply[SlotT2 <: Slot](slotId: SlotT2)(implicit ev: frame.AllSlots <:< SlotT2): slotId.type#Type
		
		def updated
			[SlotT2 <: Slot, T<:SlotT2#Type]
			(slotId: SlotT2, value:T)
			(implicit ev: frame.AllSlots <:< SlotT2)
				:FrameInstance[TFrame]

		def updatedWithUpdater
			[SlotT2 <: Slot, TIn>:SlotT2#Type, TOut<:SlotT2#Type]
			(slotId: SlotT2)(updater:TIn  => TOut)
			(implicit ev: frame.AllSlots <:< SlotT2)
				:FrameInstance[TFrame]
	}
	
	/** An interface for custom type with embedded frame instance.*/
	trait WithFrameInstance {
		type TFrame <: Frame
		type TFrameInstance = FrameInstance[TFrame] 
		def fi: TFrameInstance
	}
	
	abstract class CustomFrameInstance[TFrame2<: Frame](fi:FrameInstance[TFrame2]) extends WithFrameInstance {
		type TFrame = TFrame2 
	}
	trait TypedFrame[F<:WithFrameInstance]{
		type TFrame = F#TFrame
		type TInstance = F
		def initialValue:F
		def frame: TInstance#TFrame
		val construct: (F#TFrameInstance) => F
	}
	
	final case class FrameWithSlotsAndType[F<:WithFrameInstance](
		  override val
		  frame: F#TFrame,
		  override val
		  construct: F#TFrameInstance => F) extends TypedFrame[F]{
		
		lazy val initialValue: F = construct(frame.initialValue)
	}
	
	
	implicit def toRichWithFrameInstance[TInstance<:WithFrameInstance]
		(instance:TInstance)
		(implicit tf:TypedFrame[TInstance]) =
		new WithFrameInstanceOps[TInstance](instance, tf)

	case class FrameLens[O, V](get : V, set: (V) => O){
		def apply:V = get
		def apply(value:V):O = set(value)
	}
	
	implicit def frameLensOps[O, V](f:FrameLens[O, V]) = new FrameLensOps[O, V](f)
}
